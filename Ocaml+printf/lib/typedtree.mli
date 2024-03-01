(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TVar of int
  | TPrim of string
  | TArr of typ * typ
  | TUnit
  | TTuple of typ * typ list
  | TList of typ
  | TFString of typ

module TypeVarSet : sig
  type elt = int
  type t = Set.Make(Int).t

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

type scheme = Scheme of TypeVarSet.t * typ

val type_var : int -> typ
val int_typ : typ
val bool_typ : typ
val char_typ : typ
val string_typ : typ
val unit_typ : typ
val format_typ : typ -> typ
val arrow : typ -> typ -> typ
val ( @-> ) : typ -> typ -> typ
