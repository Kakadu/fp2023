(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

val pp_binder : Format.formatter -> binder -> unit
val show_binder : binder -> string

module VarSet : sig
  type elt = binder
  type t = Set.Make(Int).t

  val mem : elt -> t -> bool
  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
  val is_empty : t -> bool
end

type binder_set = VarSet.t

val pp_binder_set : Format.formatter -> binder_set -> unit
val show_binder_set : binder_set -> string

type ty =
  | TyLit of string
  | TyVar of binder
  | TyArrow of ty * ty
  | TyList of ty
  | TyTuple of ty list
  | TyTree of ty

type error =
  | OccursCheck
  | NoVariable of string
  | UnificationFailed of ty * ty

val pp_error : Format.formatter -> error -> unit
val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string

type scheme = S of binder_set * ty

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string
val pp_type : Format.formatter -> ty -> unit
