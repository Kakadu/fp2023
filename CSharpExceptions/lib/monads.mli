(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base_monad : sig
  type ('st, 'a, 'err) t = 'st -> 'st * ('a, 'err) result
end

module Env_Monad : sig
  type ('st, 'a) t = ('st, 'a, Errors.error) Base_monad.t
end

module Type_check_Monad : sig
  val return : 'a -> 'st -> 'st * ('a, 'err) result
  val fail : 'err -> 'st -> 'st * ('a, 'err) result

  val ( >>= )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('a -> 'st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('b, 'err) result

  val save : 'st -> 'st -> 'st * (unit, 'err) result
  val continue : ('st -> 'st * ('a, 'err) result) -> 'st -> 'st * ('a, 'err) result

  val lift2
    :  ('a -> 'b -> 'c)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('c, 'err) result

  val lift3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> ('st -> 'st * ('c, 'err) result)
    -> 'st
    -> 'st * ('d, 'err) result

  val lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> ('st -> 'st * ('c, 'err) result)
    -> ('st -> 'st * ('d, 'err) result)
    -> 'st
    -> 'st * ('e, 'err) result

  val ( *> )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('b, 'err) result

  val ( <* )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('a, 'err) result

  val ( <|> )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('a, 'err) result)
    -> 'st
    -> 'st * ('a, 'err) result

  val ( >>| )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('a -> 'b)
    -> 'st
    -> 'st * ('b, 'err) result

  val map_left
    :  ('a -> 'st -> 'st * ('b, 'err) result)
    -> 'a list
    -> 'st
    -> 'st * ('b list, 'err) result

  val iter_left
    :  ('a -> 'st -> 'st * (unit, 'err) result)
    -> 'a list
    -> 'st
    -> 'st * (unit, 'err) result

  val choice
    :  ('st, 'a, Errors.error) Base_monad.t list
    -> ('st, 'a, Errors.error) Base_monad.t

  type ctx_env = Env_types.type_check_ctx
  type 'a t = (ctx_env, 'a) Env_Monad.t

  val run : 'a t -> ctx_env * ('a, Errors.error) result
  val save_local : Env_types.t_loc_env -> unit t
  val save_local_el : Ast.ident -> Env_types.t_env_value -> unit t
  val save_global : Env_types.text -> unit t
  val save_global_el : Env_types.code_ident -> Env_types.code_ctx -> unit t
  val save_scope_tp : Ast.meth_type option -> unit t
  val save_main_ctx : Env_types.code_ident option -> unit t
  val read : ctx_env t
  val read_local : Ast.ident -> Env_types.t_env_value t
  val read_local_opt : Ast.ident -> Env_types.t_env_value option t
  val read_global : Env_types.code_ident -> Env_types.code_ctx t
  val read_global_opt : Env_types.code_ident -> Env_types.code_ctx option t
  val read_scope_tp : Ast.meth_type option t
  val read_main_ctx : Env_types.code_ident option t
end
