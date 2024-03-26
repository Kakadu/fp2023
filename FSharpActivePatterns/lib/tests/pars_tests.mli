(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib
open Ast

val start_test : 'a Angstrom.t -> ('a -> name) -> name -> unit
