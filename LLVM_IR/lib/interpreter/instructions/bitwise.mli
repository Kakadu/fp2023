(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val launch_bitwise_operation
  :  Ast.bitwise_binary_operation
  -> (Ihelp.State.state, CommonInterpInstructions.instr_launch_res) Ihelp.State.t
