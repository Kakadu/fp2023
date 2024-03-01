(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val parse_primitive_type : Ast.tp Angstrom.t
val parse_main_type : Ast.tp Angstrom.t
val parse_additional_type : Ast.tp Angstrom.t
val additional_type : Ast.tp -> Ast.tp Angstrom.t
