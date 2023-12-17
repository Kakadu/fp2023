(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

type t = Var of string  (** Type variable *)

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
