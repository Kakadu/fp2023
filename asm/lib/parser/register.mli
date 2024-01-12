(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_64_register : Ast.i64 Ast.register Angstrom.t
val parse_128_register : Ast.i128 Ast.register Angstrom.t
val between_brackets : 'a Angstrom.t -> 'a Angstrom.t
val parse_64_register_ref : Ast.i64 Ast.register_ref Angstrom.t
val parse_128_register_ref : Ast.i128 Ast.register_ref Angstrom.t
