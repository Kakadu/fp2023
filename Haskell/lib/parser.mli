(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** [plit] is a parser for literals. *)
val plit : Ast.lit Angstrom.t

(** [ppat] is a parser for patterns. *)
val ppat : Ast.pat Angstrom.t

(** [pexpr] is a parser for expressions. *)
val pexpr : Ast.expr Angstrom.t

(** [pdecl] is a parser for declarations. *)
val pdecl : Ast.decl Angstrom.t

(** [pprog] is a parser for complete programs. *)
val pprog : Ast.prog Angstrom.t

(** [parse source] attempts to parse the [source] string into an AST.
    Returns [Ok prog] on success, [Error msg] on failure. *)
val parse : string -> (Ast.prog, string) result
