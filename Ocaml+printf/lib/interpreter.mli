(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VFormat_string of Ast.fstring
  | VFun of string option * Ast.pattern * Ast.expr * env_values
  | VTuple of value * value list
  | VList of value list

and env_values = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type error =
  [ `Division_by_zero
  | `Impossible_state of string
  | `Invalid_argument of string
  | `Matching_failure
  | `No_variable
  | `Type_mismatch
  ]

val pp_error : Format.formatter -> error -> unit

module EnvValues : sig
  val std : env_values
  val find : env_values -> string -> value option
  val pp_env_values : Stdlib.Format.formatter -> env_values -> unit
end

val run_eval_program : Ast.toplevel list -> (env_values, error) result
val run_eval_expr : Ast.expr -> (value, error) result
