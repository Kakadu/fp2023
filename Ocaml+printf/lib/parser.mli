(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** creates AST from text of program *)
val run_parser_program : string -> (Ast.program, string) result

(** creates AST from expression represented by a string *)
val run_parser_expr : string -> (Ast.expr, string) result
