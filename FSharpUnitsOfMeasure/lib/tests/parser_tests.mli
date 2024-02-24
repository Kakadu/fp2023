(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast

val parsed_result : id -> 'a Angstrom.t -> ('a -> id) -> unit
