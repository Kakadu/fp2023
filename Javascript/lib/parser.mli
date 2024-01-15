(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

type error = [ `ParsingError of string ]

val parse : string -> (Ast.statement, [> error ]) result
val parse_expression : string -> (Ast.statement, [> error ]) result
