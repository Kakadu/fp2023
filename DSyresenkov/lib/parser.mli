(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse_expr : string -> (expr, string) result
val parse : string -> (expr list, string) result
