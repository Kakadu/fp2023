(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open VTypes

val interpret_ast : Ast.statement -> (value, string) result
val interpret : string -> (value, string) result
