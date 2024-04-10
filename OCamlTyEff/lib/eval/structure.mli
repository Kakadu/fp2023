(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Ast
open Values

val eval : structure_item -> (Val.t option * Ident.t list) EvalMonad.t
(**
  Evaluates structure_item.
  Returns value of the structure_item if applicable
  and list of bound identifiers
*)
