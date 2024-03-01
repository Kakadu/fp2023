(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type error =
  | DivisionByZero (** Interpret Errors*)
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | TypeError
  | Unreachable
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
  | NotImplemented -> Format.fprintf fmt "This feature has not yet been implemented"
  | OccursCheck -> Format.fprintf fmt "Typechecker error: occurs check failed"
  | NoVariable s -> Format.fprintf fmt "Typechecker error: undefined variable '%s'" s
  | UnificationFailed (l, r) ->
    Format.fprintf
      fmt
      "Typechecker error: unification failed on %a and %a"
      Typedtree.pp_typ_binder
      l
      Typedtree.pp_typ_binder
      r
  | EmptyPattern -> Format.fprintf fmt "Typechecker error: empty pattern"
;;
