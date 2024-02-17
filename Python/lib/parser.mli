(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : 'a Angstrom.t -> string -> ('a, string) result
val pyParser : Ast.statement list Angstrom.t
val parser : string -> (Ast.statement list, string) result
