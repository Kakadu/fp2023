(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Env_types.Common_env

module type MONAD = sig
  type ('st, 'a, 'err) t = 'st -> 'st * ('a, 'err) Result.t

  val return : 'a -> ('st, 'a, 'err) t
  val fail : 'err -> ('st, 'a, 'err) t
  val ( >>= ) : ('st, 'a, 'err) t -> ('a -> ('st, 'b, 'err) t) -> ('st, 'b, 'err) t
  val save : 'st -> ('st, unit, 'err) t
  val continue : ('st, 'a, 'err) t -> 'st -> 'st * ('a, 'err) Result.t
  val ( <|> ) : ('st, 'a, 'err) t -> ('st, 'a, 'err) t -> ('st, 'a, 'err) t
end

module TC_Kernel : MONAD = struct
  type ('st, 'a, 'err) t = 'st -> 'st * ('a, 'err) Result.t

  let return : 'a -> ('st, 'a, 'err) t = fun x st -> st, Result.ok x
  let fail : 'err -> ('st, 'a, 'err) t = fun er st -> st, Result.error er

  let ( >>= ) : ('st, 'a, 'err) t -> ('a -> ('st, 'b, 'err) t) -> ('st, 'b, 'err) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error s -> st1, Result.error s
  ;;

  let save : 'st -> ('st, unit, 'err) t = fun new_ctx _ -> new_ctx, Result.ok ()

  let continue : ('st, 'a, 'err) t -> 'st -> 'st * ('a, 'err) Result.t =
    fun f c_env -> f c_env
  ;;

  let ( <|> ) : ('st, 'a, 'err) t -> ('st, 'a, 'err) t -> ('st, 'a, 'err) t =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
  ;;
end

module Base_monad (M : MONAD) = struct
  open M

  type ('st, 'a, 'err) t = ('st, 'a, 'err) M.t

  let lift2
    : ('a -> 'b -> 'c) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'c, 'err) t
    =
    fun f a b st -> (a >>= fun a1 -> b >>= fun b1 -> return (f a1 b1)) st
  ;;

  let lift3
    :  ('a -> 'b -> 'c -> 'd) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t
    =
    fun f a b c st ->
    (a >>= fun a1 -> b >>= fun b1 -> c >>= fun c1 -> return (f a1 b1 c1)) st
  ;;

  let lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t -> ('st, 'e, 'err) t
    =
    fun f a b c d st ->
    lift2
      (fun (a1, b1) (c1, d1) -> f a1 b1 c1 d1)
      (lift2 (fun a1 b1 -> a1, b1) a b)
      (lift2 (fun c1 d1 -> c1, d1) c d)
      st
  ;;

  let ( *> ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'b, 'err) t =
    fun a b st -> (lift2 (fun _ x -> x) a b) st
  ;;

  let ( <* ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'a, 'err) t =
    fun a b st -> (lift2 (fun x _ -> x) a b) st
  ;;

  let ( >>| ) : ('st, 'a, 'err) t -> ('a -> 'b) -> ('st, 'b, 'err) t =
    fun x f -> x >>= fun x_res -> return (f x_res)
  ;;

  let map_left : ('a -> ('st, 'b, 'err) t) -> 'a list -> ('st, 'b list, 'err) t =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return (x :: tl) in
    List.fold_left f (return []) mlst >>| List.rev
  ;;

  let iter_left : ('a -> ('st, unit, 'err) t) -> 'a list -> ('st, unit, 'err) t =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun _ -> custom_f cur >>= fun _ -> return () in
    List.fold_left f (return ()) mlst
  ;;
end

module Env_Monad (M : MONAD) = struct
  include Base_monad (M)
  include M

  type ('st, 'a) t = ('st, 'a, error) Base_monad(M).t

  let choice : ('st, 'a) t list -> ('st, 'a) t =
    fun l ->
    match l with
    | [] -> fail (Other_error "Empty choice\n")
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;
end
(* =================================================================== *)


module I_Kernel = struct
  include TC_Kernel

  
end

module Type_check_Monad = struct
  include Env_Monad (TC_Kernel)
  open Env_types.Type_check_env

  type ctx_env = type_check_ctx
  type 'a t = (ctx_env, 'a) Env_Monad(TC_Kernel).t

  let run : 'a t -> ctx_env * ('a, error) Result.t =
    fun f -> f (CodeMap.empty, IdentMap.empty, None, None)
  ;;

  let save_local : t_loc_env -> unit t =
    fun l_env c_env ->
    let code, _, tp, main = c_env in
    (code, l_env, tp, main), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit t =
    fun id t_val c_env ->
    let _, local_env, _, _ = c_env in
    let new_l = IdentMap.add id t_val local_env in
    save_local new_l c_env
  ;;

  let save_global : text -> unit t =
    fun g_env c_env ->
    let _, local_env, tp, main = c_env in
    (g_env, local_env, tp, main), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit t =
    fun id ctx c_env ->
    let code, _, _, _ = c_env in
    let new_l = CodeMap.add id ctx code in
    save_global new_l c_env
  ;;

  let save_scope_tp : meth_type option -> unit t =
    fun new_scope_tp st ->
    let code, local_env, _, main = st in
    (code, local_env, new_scope_tp, main), Result.ok ()
  ;;

  let save_main_ctx : code_ident option -> unit t =
    fun class_with_main st ->
    let code, local_env, scope_tp, _ = st in
    (code, local_env, scope_tp, class_with_main), Result.ok ()
  ;;

  let read : ctx_env t = fun st -> (return st) st

  let read_local : ident -> t_env_value t =
    fun id c_env ->
    let _, local_env, _, _ = c_env in
    match IdentMap.find_opt id local_env with
    | Some x -> return x c_env
    | None -> fail (Not_find_ident_of id) c_env
  ;;

  let read_local_opt : ident -> t_env_value option t =
    fun id c_env -> (read_local id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_global : code_ident -> code_ctx t =
    fun id c_env ->
    let id_ =
      match id with
      | Code_ident x -> x
    in
    let code, _, _, _ = c_env in
    match CodeMap.find_opt id code with
    | Some x -> return x c_env
    | None -> fail (Not_find_ident_of id_) c_env
  ;;

  let read_global_opt : code_ident -> code_ctx option t =
    fun id c_env -> (read_global id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_scope_tp : meth_type option t =
    fun st ->
    match st with
    | _, _, scope_tp, _ -> (return scope_tp) st
  ;;

  let read_main_ctx : code_ident option t =
    fun st ->
    let _, _, _, id = st in
    (return id) st
  ;;
end

module Eval_Monad = struct
  include Env_Monad (TC_Kernel)
  open Env_types.Eval_env

  type ctx_env = interpret_ctx
  type 'a t = (interpret_ctx, 'a tp_return) Env_Monad(TC_Kernel).t

  let old_return = return
  let return : 'a -> 'a t = fun elem st -> old_return (to_info elem) st
  let return_res : 'a option -> 'a t = fun elem st -> old_return (to_return elem) st
  let return_break : 'a t = fun st -> old_return Break st

  let get_class : code_ident -> code_ctx t =
    fun id st ->
    let code, _, _, _ = st in
    let (Code_ident id_) = id in
    match CodeMap.find_opt id code with
    | Some x -> return x st
    | None -> fail (Not_find_ident_of id_) st
  ;;

  let get_memory_el : address -> mem_el t =
    fun ad st ->
    let _, _, (_, memory_map), _ = st in
    match MemMap.find_opt ad memory_map with
    | Some x -> return x st
    | None -> fail Non_existent_address st
  ;;

  (* let get_local_elem: ident -> code_ctx t =
     fun id st ->
     let _, (ad, l_env), _, _ = st in
     let get_from_env = match IdentMap.find_opt id l_env with
     | Some x -> return x st
     | None -> fail(Not_find_ident) st
     in
     let get_from_mem = get_memory_el ad in

     ;; *)

  (* let *)
end
