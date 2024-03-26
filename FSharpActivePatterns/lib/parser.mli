(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

val start_parsing : 'a t -> string -> ('a, string) result
val parse_const : const t
val parse_var : pattern t
val pat : pattern t
val parse : expr t
val main_parse : string -> (Ast.expr list, string) result
