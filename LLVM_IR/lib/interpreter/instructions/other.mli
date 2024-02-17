(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val iicmp
  :  Ast.variable
  -> string
  -> Ast.tp
  -> Ast.value
  -> Ast.value
  -> (Ihelp.State.state, unit) Ihelp.State.t

val ifcmp
  :  Ast.variable
  -> string
  -> Ast.tp
  -> Ast.value
  -> Ast.value
  -> (Ihelp.State.state, unit) Ihelp.State.t

val iphi
  :  Ast.variable
  -> 'a
  -> (Ast.value * Ast.value) list
  -> (Ihelp.State.state, unit) Ihelp.State.t

val iselect
  :  Ast.variable
  -> Ast.tp
  -> Ast.value
  -> Ast.tp
  -> Ast.value
  -> Ast.value
  -> (Ihelp.State.state, unit) Ihelp.State.t
