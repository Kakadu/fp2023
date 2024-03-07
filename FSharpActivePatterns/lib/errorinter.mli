(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typeandprinter

type error_infer =
  | OccursCheck (** Typing errors *)
  | NoVariable of string
  | UnificationFailed of typ * typ
  | EmptyPattern
  | EmptyProgram
  | NotImplemented

type error_inter =
  | DivisionByZero (** Interpret Errors*)
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | TypeError
  | Unreachable
  | StringOfLengthZero of string
  | EmptyProgram
  | NotImplemented

val pp_error_infer : Format.formatter -> error_infer -> unit
val pp_error_inter : Format.formatter -> error_inter -> unit
