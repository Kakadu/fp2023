(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

(** creates AST from text of program *)
val parse_request : string -> (Ast.clause, string) result
