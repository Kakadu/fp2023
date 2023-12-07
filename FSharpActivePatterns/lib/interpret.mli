(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val eval_program : Ast.program -> (Ast.value, Errorinter.error) result
