(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val interpreter : string -> (State_type.St_Interpreter.vl option, Monad.error) result
