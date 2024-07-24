(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)
val final_parse : Ast.statement list Angstrom.t

val parser_result_to_stmt_list : string -> Ast.statement list
val interpret_parse : 'a Angstrom.t -> ('a -> string) -> string -> unit
