(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Misc

(** Type environment. Maps identifiers to types *)
type t

(** Maps identifiers of defined types / type constructors to their arity *)
type types_arity = (Ident.t, int, Ident.comparator_witness) Map.t

val get_types_arity : t -> types_arity
val set_types_arity : t -> types_arity -> t

val get_weak_counter : t -> int
val set_weak_counter : t -> int -> t

val empty : t

val idents : t -> Ident.t list
val set : t -> key:Ident.t -> data:Scheme.t -> t
val map : t -> f:(Scheme.t -> Scheme.t) -> t
val find : t -> Ident.t -> Scheme.t option
val find_exn : t -> Ident.t -> Scheme.t
