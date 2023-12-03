open! Base

(** Set of type variables *)
type t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

val empty : t

val singleton : Var.t -> t

val add : t -> Var.t -> t

val union : t -> t -> t

val union_list : t list -> t

val diff : t -> t -> t
