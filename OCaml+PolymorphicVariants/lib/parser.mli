(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.structure, string) result
val test_parse : string -> unit
