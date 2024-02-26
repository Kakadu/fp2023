(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(**run print ok result of parser or raise error otherwise*)
val pp_ok : (Ast.statement, 'a) result -> unit

(**print error of parser or raise error otherwise*)
val pp_error : ('a, [< `ParsingError of string ]) result -> unit

(**run parser 'parse' and print result or error*)
val pp : ?parse:(string -> (Ast.statement, Parser.error) result) -> string -> unit

(**parse and interpret js code then print result of return statement*)
val print_return : string -> unit

(**parse and interpret js code then print result of return statement and stdout of program*)
val print_output : string -> unit

(**parse and interpret js code then print stdout of program*)
val print_stdout : string -> unit
