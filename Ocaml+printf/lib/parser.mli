(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** parses a program *)
val program_parser : Ast.program Angstrom.t

(** skips whitespaces before and parses name of value as expression *)
val expr_valname : Ast.expr Angstrom.t

(** skips whitespaces before and parses integer as expression *)
val expr_integer : Ast.expr Angstrom.t

(** [parenthesis p] creates a parser that will parses a something enclosed in parentheses by running [p].
    Skips whitespaces before and between *)
val parenthesis : 'a Angstrom.t -> 'a Angstrom.t

(** [unary_op p] creates a parser that will parses all unary operatorls before expression and then runs [p].
    Skips whitespaces before and between. If there are no unary operators before expression, returns [p] *)
val unary_op : Ast.expr Angstrom.t -> Ast.expr Angstrom.t

(** [if_then_else p] creates a parser that will parses if-then-else expression by running [p] to parse statement and branches.
    Skips whitespaces before and between. Supports nesting. If there are no one if-then-else expression, returns [p] *)
val if_then_else : Ast.expr Angstrom.t -> Ast.expr Angstrom.t

(** parses an expression *)
val expr : Ast.expr Angstrom.t

(** parses a toplevel let-binding *)
val decl : Ast.let_decl Angstrom.t

(** creates AST from text of program *)
val parse : string -> (Ast.program, string) result
