(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val launch_unary_operation
  :  Ast.unary_operation
  -> (Ihelp.State.state, CommonInterpInstructions.instr_launch_res) Ihelp.State.t
