(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

type primitives =
  | Int
  | String
  | Char
  | Bool
  | Unit

type typ =
  | TVar of type_var
  | TPrim of primitives
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ

module VarSet : sig
  type elt = type_var
  type t = Set.Make(Int).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

type scheme = Scheme of VarSet.t * typ

type error =
  | OccursCheck
  | UndeclaredVariable of string
  | UnificationFailed of typ * typ
  | NotImplemented

val tvar : type_var -> typ
val tint : typ
val tbool : typ
val tchar : typ
val tunit : typ
val tstring : typ
val tarrow : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ
val ( @-> ) : typ -> typ -> typ
