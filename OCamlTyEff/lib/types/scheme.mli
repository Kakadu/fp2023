(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

(** Type with universally quantified type variables *)
type t = Forall of VarSet.t * Ty.t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t

val free_vars : t -> VarSet.t
(** Free type variables in scheme *)
