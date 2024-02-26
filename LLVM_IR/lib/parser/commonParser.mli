(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val test_parse_res : 'a Angstrom.t -> string -> ('a, string) result
val test_parse : 'a Angstrom.t -> ('a -> string) -> string -> unit
val whitespaces : unit list Angstrom.t
val comma : char Angstrom.t
val parse_integer : int Angstrom.t
val parse_integer64 : Int64.t Angstrom.t
val parse_word : string Angstrom.t
val word : string -> string Angstrom.t
val parse_name : string Angstrom.t
val parse_local_variable : Ast.variable Angstrom.t
val parse_global_variable : Ast.variable Angstrom.t
