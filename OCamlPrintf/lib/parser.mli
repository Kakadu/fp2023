(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** parser of program *)
val program_parser : Ast.let_decl list Angstrom.t

(** creates AST from text of program *)
val parse : string -> (Ast.let_decl list, string) result
