(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module Var : sig
  type t = Var of string | Var_weak of string  (** Type / effect variable *)

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  include Comparator.S with type t := t
end

module VarSet : sig
  module Elt : sig
    type t = Var_ty of Var.t | Var_eff of Var.t

    val equal : t -> t -> bool
  end

  (** Set of type and effect variables *)
  type t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val empty : t
  val singleton : Elt.t -> t
  val singleton_ty : Var.t -> t
  val singleton_eff : Var.t -> t
  val of_list : Elt.t list -> t
  val to_list : t -> Elt.t list

  val is_empty : t -> bool
  val mem : t -> Elt.t -> bool

  val add : t -> Elt.t -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t

  val filter : t -> f:(Elt.t -> bool) -> t
  val fold : t -> init:'acc -> f:('acc -> Elt.t -> 'acc) -> 'acc
  val fold_right : t -> init:'acc -> f:(Elt.t -> 'acc -> 'acc) -> 'acc
end
