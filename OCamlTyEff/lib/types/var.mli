open! Base

type t = Var of string  (** Type variable *)

val pp : Format.formatter -> t -> unit

val show : t -> string

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
