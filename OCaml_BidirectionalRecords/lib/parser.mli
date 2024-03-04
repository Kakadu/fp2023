(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pexpr : Ast.expr Angstrom.t
val parse_expr : string -> (Ast.expr, string) result
