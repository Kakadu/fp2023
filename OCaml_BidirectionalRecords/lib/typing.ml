(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

type primitives =
  | Int
  | String
  | Char
  | Bool
  | Unit
[@@deriving show { with_path = false }]

type typ =
  | TVar of type_var (* 'a *)
  | TPrim of primitives (* int, bool, etc. *)
  | TArr of typ * typ (* 'a -> 'a *)
  | TTuple of typ list (* (int, string) *)
  | TList of typ (* int list *)

module VarSet = Stdlib.Set.Make (Int)

type scheme = Scheme of VarSet.t * typ

type error =
  | OccursCheck
  | UndeclaredVariable of string
  | UnificationFailed of typ * typ
  | NotImplemented

let tvar x = TVar x
let tint = TPrim Int
let tbool = TPrim Bool
let tchar = TPrim Char
let tunit = TPrim Unit
let tstring = TPrim String
let tarrow l r = TArr (l, r)
let ttuple x = TTuple x
let tlist x = TList x
let ( @-> ) = tarrow
