(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Set of variables *)
type id = string [@@deriving show { with_path = false }]

module VarSet : Set.S with type elt = int

(* Base types *)
type base_type =
  | BInt (** Basic integer type *)
  | BBool (** Basic bool type *)
  | BUnit (** Unit type *)
[@@deriving show { with_path = false }]

(* Types *)
type ty =
  | TBase of base_type (** Type of integer *)
  | TVar of int
  | TArrow of ty * ty (** Type of function ty1 -> ty2 *)
  | TTuple of ty * ty * ty list (** Type of tuple *)
  | TList of ty (** Type of list *)
[@@deriving show { with_path = false }]

type error =
  | OccursCheckFailed of int * ty (** OCaml's Occurs check *)
  | NoVariable of id (** Attempt to use non-initialized variable *)
  | UnificationFailed of ty * ty (** Failed to unify left and right types *)
  | PatternRebound (** Attempt to use in pattern single bound two times (h :: h :: tl) *)
  | NotImplemented (** Still not implemented features *)
[@@deriving show { with_path = false }]

val pp_type : Format.formatter -> ty -> unit
val print_ty : ty -> unit

type scheme = S of VarSet.t * ty
