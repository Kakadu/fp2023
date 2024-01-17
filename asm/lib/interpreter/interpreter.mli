(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val eval_ast : Ast.statement list -> (State.GlobalState.t, unit) State.StateErrorMonad.t

module State = State
