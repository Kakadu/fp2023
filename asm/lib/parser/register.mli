(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_32_register : Ast.i32 Ast.register Angstrom.t
val parse_64_register : Ast.i64 Ast.register Angstrom.t
val parse_32_register_ref : Ast.i32 Ast.register_ref Angstrom.t
val parse_64_register_ref : Ast.i64 Ast.register_ref Angstrom.t
