(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val parse_const : Ast.tp -> Ast.const Angstrom.t
val parse_value : Ast.tp -> Ast.value Angstrom.t
val parse_type_with_value : (Ast.tp * Ast.value) Angstrom.t
val parse_type_with_value2 : (Ast.tp * Ast.value * Ast.value) Angstrom.t
val type_with_value : Ast.tp -> Ast.value Angstrom.t
