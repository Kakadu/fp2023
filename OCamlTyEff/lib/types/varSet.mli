(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

(** Set of type variables *)
type t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t

val empty : t
val singleton : Var.t -> t
val of_list : Var.t list -> t

val is_empty : t -> bool
val mem : t -> Var.t -> bool

val add : t -> Var.t -> t
val union : t -> t -> t
val union_list : t list -> t
val inter : t -> t -> t
val diff : t -> t -> t

val fold : t -> init:'acc -> f:('acc -> Var.t -> 'acc) -> 'acc
val fold_right : t -> init:'acc -> f:(Var.t -> 'acc -> 'acc) -> 'acc
