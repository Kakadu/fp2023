(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom
open Ast

(** Run parser on string and pretty print the output using printer.
    Used for inline expect tests *)
val pp : (Stdlib.Format.formatter -> 'a -> unit) -> 'a t -> string -> unit

(** skip whitespaces *)
val ws : unit t

val parse_ident : ident t
val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
