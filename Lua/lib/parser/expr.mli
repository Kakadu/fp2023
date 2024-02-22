(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom
open Ast

val parse_expr : block t -> expression t
val parse_function : block t -> expression t
val parse_apply : expression t -> apply t
