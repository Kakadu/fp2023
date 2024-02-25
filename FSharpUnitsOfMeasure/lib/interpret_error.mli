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

val print_error : Format.formatter -> error -> unit
