(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

(** SQL-values parser *)
val value_p : Ast.value Angstrom.t

(** Arithm epxprs parser *)
val arithm_p : Ast.expr Angstrom.t

(** Compare expressions parser *)
val cmp_p : Ast.expr Angstrom.t

(** Logic expressions parser *)
val logic_p : Ast.expr Angstrom.t

(** Expressions parser *)
val expr_p : Ast.expr Angstrom.t

(** SELECT-statement parser *)
val select_p : Ast.select_statement list Angstrom.t

(** ON-statement parser *)
val on_p : Ast.expr Angstrom.t

(** JOIN-statement parser *)
val join_p : Ast.from_statement Angstrom.t

(** FROM-statement parser *)
val from_p : Ast.from_statement Angstrom.t

(** SQL-statements (angstrom) parser *)
val request_p : Ast.request Angstrom.t

(** SQL-statements parser *)
val parse : string -> (Ast.request, string) result
