(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

type error = [ `ParsingError of string ]

val chainl1 : Ast.term Angstrom.t -> Ast.atom Angstrom.t -> Ast.term Angstrom.t

(*Parsers*)
val atom : Ast.atom Angstrom.t
val const : Ast.const Angstrom.t
val var : Ast.term Angstrom.t
val relation : Ast.term Angstrom.t
val term : Ast.term Angstrom.t
val many_term_c : Ast.term list -> Ast.many_term
val parse_prolog : Ast.many_term Angstrom.t
val parse_program : string -> unit
