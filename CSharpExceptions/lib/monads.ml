(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Env_types.Common_env

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

  let ( <|> ) : ('st, 'a, 'err) t -> ('st, 'a, 'err) t -> ('st, 'a, 'err) t =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
  ;;

  let lift2
    : ('a -> 'b -> 'c) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'c, 'err) t
    =
    fun f a b -> a >>= fun a1 -> b >>= fun b1 -> return (f a1 b1)
  ;;

  let lift3
    :  ('a -> 'b -> 'c -> 'd) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t
    =
    fun f a b c -> a >>= fun a1 -> b >>= fun b1 -> c >>= fun c1 -> return (f a1 b1 c1)
  ;;

  let lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t -> ('st, 'e, 'err) t
    =
    fun f a b c d ->
    lift2
      (fun (a1, b1) (c1, d1) -> f a1 b1 c1 d1)
      (lift2 (fun a1 b1 -> a1, b1) a b)
      (lift2 (fun c1 d1 -> c1, d1) c d)
  ;;

  let ( *> ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'b, 'err) t =
    fun a b -> lift2 (fun _ x -> x) a b
  ;;

  let ( <* ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'a, 'err) t =
    fun a b -> lift2 (fun x _ -> x) a b
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

module Type_check_Monad = struct
  open Env_types.Type_check_env
  include Base_monad

  type ctx_env = type_check_ctx
  type 'a t = ctx_env -> ctx_env * ('a, error) Result.t

  let choice : 'a t list -> 'a t =
    fun l ->
    match l with
    | [] -> fail (Other_error "Empty choice\n")
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;

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
  open Env_types.Eval_env
  include Base_monad

  type ctx_env = interpret_ctx
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, error) signal

  let return_n : 'a -> ('a, 'b) t = fun x st -> st, nsig x
  let return_r : 'b option -> ('a, 'b) t = fun x st -> st, rsig x
  let return_b : 'c -> ('a, 'b) t = fun _ st -> st, bsig
  let return_e : code_ident -> address -> ('a, 'b) t = fun id ad st -> st, esig id ad
  let fail : error -> ('a, 'b) t = fun er st -> st, error er

  let ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Next x -> f x st1
    | Return x -> return_r x st1
    | Exn (id, ad) -> return_e id ad st1
    | Break -> return_b () st1
    | Error err -> fail err st1
  ;;

  let ( |>>= ) : ('a, 'c) t -> ('c option -> ('b, 'd) t) -> ('b, 'd) t =
    (* TODO: как-то сделать обертку, чтоб в конце каждого вызова метода запускался [return_r None], если не было вызвано другого return *)
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Return x -> f x st1
    | Exn (id, ad) -> return_e id ad st1
    | Error err -> fail err st1
    | _ ->
      fail
        (Type_check_error
           "If the method is not of 'void' type, then it must have 'return'")
        st1
  ;;

  let ( !>>= ) : ('a, 'c) t -> (code_ident * address -> ('b, 'd) t) -> ('b, 'd) t =
    (* TODO:
       чтоб обрабатывать 'finally' нужна фигня типа:
       (try !>>= catch |>>= fun x -> finally *> return_r x)
       * помни, что |>>= срабатывает после получения Return или продолжает падать с ошибкой
    *)
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Exn (id, ad) -> f (id, ad) st1
    | Return x -> return_r x st1
    | Next x -> return_n x st1
    | Break -> return_b () st1
    | Error err -> fail err st1
  ;;

  let ( @>>= ) : (unit, 'b) t -> (unit -> ('c, 'd) t) -> ('c, 'd) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Exn (id, ad) -> return_e id ad st1
    | Return x -> return_r x st1
    | Next _ | Break -> f () st1
    | Error err -> fail err st1
  ;;

  let ( <|> ) : ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t =
    fun a1 a2 st ->
    let st1, x1 = a1 st in
    match x1 with
    | Next x -> return_n x st1
    | Return x -> return_r x st1
    | Exn (id, ad) -> return_e id ad st1
    | Break -> return_b () st1
    | Error _ -> a2 st
  ;;

  let save : ctx_env -> (unit, 'b) t = fun new_ctx _ -> new_ctx, nsig ()
  let read : (ctx_env, 'c) t = fun st -> (return_n st) st

  let run : ('a, 'b) t -> ctx_env * ('a, 'b, error) signal =
    (* TODO: переписать, чтоб получало на вход class с main, и на его основе запускался *)
    fun f -> f (CodeMap.empty, (ln 0, IdentMap.empty), (ln 0, MemMap.empty), [])
  ;;

  let ( >>| ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t =
    fun x f -> x >>= fun x_res -> return_n (f x_res)
  ;;

  let ( *> ) : ('a, 'd) t -> ('b, 'c) t -> ('b, 'c) t =
    fun a b -> a >>= fun _ -> b >>= fun b1 -> return_n b1
  ;;

  let map_left : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return_n (x :: tl) in
    List.fold_left f (return_n []) mlst >>| List.rev
  ;;

  let iter_left : ('a -> (unit, 'c) t) -> 'a list -> (unit, 'c) t =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun _ -> custom_f cur >>= fun _ -> return_n () in
    List.fold_left f (return_n ()) mlst
  ;;
end
