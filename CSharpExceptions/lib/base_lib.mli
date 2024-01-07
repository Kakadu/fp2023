(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Description of the API for opening and writing to a file. *)
val fileInfo_name : Ast.ident

val fileInfo_decl : Ast.class_decl option

(** Description of the base exception. *)
val exception_name : Ast.ident

val exception_decl : Ast.class_decl option
