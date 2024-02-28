(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type error =
  | Division_by_zero
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | EmptyProgram
  | TypeError
  | Unreachable
  | NotImplemented
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ
  | Empty_pattern

val pp_error : Format.formatter -> error -> unit
