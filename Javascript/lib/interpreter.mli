(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type program_return =
  { stdout : string
  ; return : string
  }

val interpret_ast : Ast.statement -> (program_return, string) result
val interpret : string -> (program_return, string) result
