(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

type t = Ident of string  (** Identifiers *)

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t
include Comparator.S with type t := t
