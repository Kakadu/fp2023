(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_ok : (Ast.statement, 'a) result -> unit
val pp_error : ('a, [< `ParsingError of string ]) result -> unit
val pp : ?parse:(string -> (Ast.statement, Parser.error) result) -> string -> unit
val print_return : string -> unit
val print_output : string -> unit
