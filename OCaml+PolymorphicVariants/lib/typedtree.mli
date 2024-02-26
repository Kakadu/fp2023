(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty

val pp_typ : Format.formatter -> ty -> unit
val arrow : ty -> ty -> ty
val int_typ : ty
val bool_typ : ty
val string_typ : ty
val unit_typ : ty
val tuple_typ : ty list -> ty
val list_typ : ty -> ty
