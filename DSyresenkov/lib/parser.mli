(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* let rec fac x = if x = 1 then x else x * (fac x) *)

open Ast

val parse : string -> (expr, string) result
