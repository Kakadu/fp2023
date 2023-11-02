(** Copyright 2023 Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

val parse_exn : string -> Ast.structure
(** Raises [Failure] exception if parsing fails *)
val parse : string -> Ast.structure option
(** Tries to parse [string]. Returns [None] if parsing fails *)
