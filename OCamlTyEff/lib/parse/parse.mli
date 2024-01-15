(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

val parse : string -> Ast.structure option
(** Tries to parse [string]. Returns [None] if parsing fails *)

val parse_ty_exn : string -> Types.Ty.t
(** Tries to parse [string]. Raises [Failure] exception if parsing fails *)
