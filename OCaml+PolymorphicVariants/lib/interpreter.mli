(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of Ast.pattern * Ast.rec_flag * Ast.expr * environment

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type error =
  [ `Division_by_zero
  | `Not_implemented
  | `Pattern_mathing_failed
  | `Unbound_variable of string
  | `Wrong_type of value
  ]

val pp_error : Format.formatter -> error -> unit
val vint : int -> value
val vbool : bool -> value
val vstring : string -> value
val vunit : value
val vlist : value list -> value
val vtuple : value list -> value
val vfun : Ast.pattern -> Ast.rec_flag -> Ast.expr -> environment -> value

module Inter : sig
  val eval_structure : Ast.structure -> (environment, error) result
end

val test_interpret : string -> unit
