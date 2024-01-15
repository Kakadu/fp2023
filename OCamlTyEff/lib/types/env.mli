(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Ast

(** Type environment. Maps identifiers to types *)
type t

val of_alist_exn : weak_counter:int -> (Ident.t * Scheme.t) list -> t

val get_weak_counter : t -> int
val set_weak_counter : t -> int -> t

val set : t -> key:Ident.t -> data:Scheme.t -> t
val map : t -> f:(Scheme.t -> Scheme.t) -> t
val find : t -> Ident.t -> Scheme.t option
val find_exn : t -> Ident.t -> Scheme.t
