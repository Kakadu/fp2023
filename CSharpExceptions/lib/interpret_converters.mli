(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common_types
open Env_types.Eval_env
open Monads.Eval

val is_init_v : t_env_value -> (t_env_value, 'a) t
val is_assignable : t_env_value -> (t_env_value, 'a) t
val is_const_v : t_env_value -> (iconst, 'a) t
val is_inst_v : t_env_value -> (iconst, 'a) t
val is_v : t_env_value -> (iconst, 'a) t
val is_not_null_const_v : t_env_value -> (iconst, 'a) t
val is_not_null_inst_v : t_env_value -> (iconst, 'a) t
val get_params_id : Ast.params -> Ast.ident list
val is_env_code : t_env_value -> (instructions, 'a) t
val get_int : t_env_value -> (int, 'a) t
val ret_int : int -> (t_env_value, 'a) t
val update_int : (int -> int) -> t_env_value -> (t_env_value, 'a) t
val get_bool : t_env_value -> (bool, 'a) t
val ret_bool : bool -> (t_env_value, 'a) t
val update_bool : (bool -> bool) -> t_env_value -> (t_env_value, 'a) t
val get_string : t_env_value -> (string, 'a) t
val ret_string : string -> (t_env_value, 'a) t
val get_char : t_env_value -> (char, 'a) t
val ret_char : char -> (t_env_value, 'a) t
val get_inst : t_env_value -> (address, 'a) t
val ret_inst : address -> (t_env_value, 'a) t
