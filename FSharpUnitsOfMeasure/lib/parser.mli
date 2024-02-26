(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

val parser : string -> (expression list, string) result
val parse_str : 'a t -> string -> ('a, string) result
val parse_types : types t
val parse_pattern : pattern t
val parse_expression : expression t
