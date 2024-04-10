(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type program_return =
  { stdout : string
  ; return : string
  }

(**get ast and interpret it*)
val interpret_ast : Ast.statement -> (program_return, string) result

(**get Javascript code, parse and interpret it*)
val interpret : string -> (program_return, string) result
