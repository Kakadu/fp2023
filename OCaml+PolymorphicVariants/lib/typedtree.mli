(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

module VarSet : sig
  type elt = binder
  type t = Set.Make(Int).t

  val empty : t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

type binder_set = VarSet.t

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty

type scheme = S of binder_set * ty

val arrow : ty -> ty -> ty
val int_typ : ty
val bool_typ : ty
val string_typ : ty
val unit_typ : ty
val tuple_typ : ty list -> ty
val list_typ : ty -> ty
val pp_typ : Format.formatter -> ty -> unit
