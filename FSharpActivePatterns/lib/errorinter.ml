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

let pp_error fmt = function
  | DivisionByZero -> Format.fprintf fmt "Exception: Division_by_zero."
  | UnboundValue s -> Format.fprintf fmt "Error: Unbound value %s" s
  | UnboundConstructor s -> Format.fprintf fmt "Error: Unbound constructor %s" s
  | MatchFailure ->
    Format.fprintf fmt "Exception: this pattern-matching is not exhaustive."
  | EmptyProgram -> Format.fprintf fmt "Error: the program was not provided or was empty"
  | TypeError -> Format.fprintf fmt "Error: type mismatch, a different type was expected"
  | Unreachable ->
    Format.fprintf fmt "Error: Unreachable error... Something went wrong..."
  | StringOfLengthZero name -> Format.fprintf fmt "It must not be of length zero: %s" name
  | NotImplemented -> Format.fprintf fmt "This feature has not yet been implemented"
  | OccursCheck -> Format.fprintf fmt "Typechecker error: occurs check failed"
  | NoVariable s -> Format.fprintf fmt "Typechecker error: undefined variable '%s'" s
  | UnificationFailed (l, r) ->
    Format.fprintf
      fmt
      "Typechecker error: unification failed on %a and %a"
      Typeandprinter.pp_typ_binder
      l
      Typeandprinter.pp_typ_binder
      r
  | EmptyPattern -> Format.fprintf fmt "Typechecker error: empty pattern"
;;
