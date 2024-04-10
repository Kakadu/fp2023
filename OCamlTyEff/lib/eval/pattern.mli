(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Values

val match_failure : EvalError.t

val eval : pattern -> Val.t -> Bounds.t EvalMonad.t
