(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val run_interpretate_on_ast
  :  Ast.glob_list
  -> Ihelp.State.state * (Ast.const, string) result
