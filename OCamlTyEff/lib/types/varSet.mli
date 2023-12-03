open! Base

(** Set of type variables *)
type t

val empty : t

val singleton : Var.t -> t

val add : t -> Var.t -> t

val union : t -> t -> t

val union_list : t list -> t

val diff : t -> t -> t
