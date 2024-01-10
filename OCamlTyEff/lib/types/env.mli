(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Ast

(** Type environment. Maps identifiers to types *)
type t

val empty : t
val singleton : Ident.t -> Scheme.t -> t
val of_alist_exn : (Ident.t * Scheme.t) list -> t

val set : t -> key:Ident.t -> data:Scheme.t -> t
val find : t -> Ident.t -> Scheme.t option
val find_exn : t -> Ident.t -> Scheme.t
