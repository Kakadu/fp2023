(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser_errors

val print_parser_error : error -> unit
val print_parser_result : Ast.program -> unit
