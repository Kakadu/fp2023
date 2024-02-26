(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_ast : Ast.statement list Angstrom.t
val parse : 'a Angstrom.t -> string -> ('a, string) result
val parse_show : 'a Angstrom.t -> ('a -> string) -> string -> unit
