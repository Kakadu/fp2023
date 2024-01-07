(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base_monad = struct
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

  let ( <|> ) : ('st, 'a, 'err) t -> ('st, 'a, 'err) t -> ('st, 'a, 'err) t =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
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

module Env_Monad = struct
  include Base_monad
  open Errors

  type ('st, 'a) t = ('st, 'a, error) Base_monad.t

  let choice : ('st, 'a) t list -> ('st, 'a) t =
    fun l ->
    match l with
    | [] -> fail (Other_error "Empty choice\n")
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;
end

module Type_check_Monad = struct
  include Env_Monad
  open Env_types
  open Errors
  open Ast

  type ctx_env = type_check_ctx
  type 'a t = (ctx_env, 'a) Env_Monad.t

  let run : 'a t -> ctx_env * ('a, error) Result.t =
    fun f -> f (CodeMap.empty, IdentMap.empty, None, None)
  ;;

  let save_local : t_loc_env -> unit t =
    fun l_env c_env ->
    match c_env with
    | code, _, tp, main -> (code, l_env, tp, main), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit t =
    fun id t_val c_env ->
    match c_env with
    | _, local_env, _, _ ->
      let new_l = IdentMap.add id t_val local_env in
      save_local new_l c_env
  ;;

  let save_global : text -> unit t =
    fun g_env c_env ->
    match c_env with
    | _, local_env, tp, main -> (g_env, local_env, tp, main), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit t =
    fun id ctx c_env ->
    match c_env with
    | code, _, _, _ ->
      let new_l = CodeMap.add id ctx code in
      save_global new_l c_env
  ;;

  let save_scope_tp : meth_type option -> unit t =
    fun new_scope_tp st ->
    match st with
    | code, local_env, _, main -> (code, local_env, new_scope_tp, main), Result.ok ()
  ;;

  let save_main_ctx : code_ident option -> unit t =
    fun class_with_main st ->
    match st with
    | code, local_env, scope_tp, _ ->
      (code, local_env, scope_tp, class_with_main), Result.ok ()
  ;;

  let read : ctx_env t = fun st -> (return st) st

  let read_local : ident -> t_env_value t =
    fun id c_env ->
    match c_env with
    | _, local_env, _, _ ->
      (match IdentMap.find_opt id local_env with
       | Some x -> return x c_env
       | None -> fail (Not_find_ident_of id) c_env)
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
    match c_env with
    | code, _, _, _ ->
      (match CodeMap.find_opt id code with
       | Some x -> return x c_env
       | None -> fail (Not_find_ident_of id_) c_env)
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
    match st with
    | _, _, _, id -> (return id) st
  ;;
end
