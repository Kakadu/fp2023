(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** creates AST from text of program *)
val parse_program : string -> (Ast.program, string) result
