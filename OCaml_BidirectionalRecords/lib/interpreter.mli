(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | DivisionByZero
  | UnboundValue of string
  | TypeMismatch
  | NotImplemented
  | PatternMatchingFailed

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VUnit
  | VNil
  | VTuple of value list
  | VList of value list
  | VFun of Ast.pattern * Ast.expr * binding

and binding = (string, value, Base.String.comparator_witness) Base.Map.t

module R : sig
  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err
end

val run_expr_interpreter : Ast.expr -> (value, error) R.t

module PP : sig
  val pp_value : Format.formatter -> value -> unit
  val pp_error : Format.formatter -> error -> unit
  val print_value : value -> unit
  val print_error : error -> unit
end
