(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Set of variables *)
type id = string [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

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
  | OccursCheckFailed
  | NoVariable of id
  | UnificationFailed of ty * ty
  | PatternRebound
  | NotImplemented
[@@deriving show { with_path = false }]

type scheme = S of VarSet.t * ty
