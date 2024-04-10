(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Values

val eval_let_rec : value_binding list -> Bounds.t EvalMonad.t

val eval : expression -> Val.t EvalMonad.t
