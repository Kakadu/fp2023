(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val method_invoke : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val s_declaration : Ast.statement Angstrom.t
val p_body : Ast.statement Angstrom.t
val p_method : Ast.methods Angstrom.t
val c_method : Ast.member Angstrom.t
val p_field : Ast.field Angstrom.t
val p_class : Ast.objects Angstrom.t
val p_interface : Ast.objects Angstrom.t
val p_ast : Ast.program Angstrom.t
val parse : 'a Angstrom.t -> string -> ('a, string) result
val parse_to_some : 'a Angstrom.t -> string -> 'a option
val assign_option : Ast.expr option Angstrom.t
