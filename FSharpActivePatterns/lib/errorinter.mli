(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typeandprinter

type error =
  | DivisionByZero (** Interpret Errors*)
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | TypeError
  | Unreachable
  | StringOfLengthZero of string
  | OccursCheck (** Typing errors *)
  | NoVariable of string
  | UnificationFailed of typ * typ
  | EmptyPattern
  | EmptyProgram
  | NotImplemented

val pp_error : Format.formatter -> error -> unit
