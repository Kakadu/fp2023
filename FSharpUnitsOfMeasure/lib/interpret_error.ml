(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | DivideByZeroException
  | UndefinedValue of string
  | UndefinedType of string
  | DifferentMeasure of string * string
  | UnexpectedInfix
  | UnsupportedOperation
  | UnexpectedPattern
  | Unreachable

let print_error e : error -> unit =
  let open Format in
  function
  | DivideByZeroException -> fprintf e "Attempted to divide by zero"
  | UndefinedValue v -> fprintf e "The value or constructor '%s' is not defined" v
  | UndefinedType t -> fprintf e "The type '%s' is not defined" t
  | DifferentMeasure (m1, m2) ->
    fprintf e "The unit of measure '%s' does not match the unit of measure '%s'" m1 m2
  | UnexpectedInfix -> fprintf e "Unexpected infix operator in type definition"
  | UnsupportedOperation -> fprintf e "This type does not support this operator"
  | UnexpectedPattern -> fprintf e "Unexpected in pattern"
  | Unreachable -> fprintf e "Unreachable"
;;
