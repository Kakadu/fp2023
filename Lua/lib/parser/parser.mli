(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Ast

val parse : string -> (block, string) Result.t
val parse_exn : string -> block
