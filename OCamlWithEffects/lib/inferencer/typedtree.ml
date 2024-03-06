(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

type primitive_type =
  | Int
  | Bool
  | Unit
  | Char
  | String
[@@deriving eq, show { with_path = false }]

type typ =
  | TPrim of primitive_type (* int, bool, unit, char, string *)
  | TVar of type_var (* 'a, 'b, etc. *)
  | TArr of typ * typ (* 'a -> 'b *)
  | TTuple of typ list (* 'a * int * char *)
  | TList of typ (* 'a list *)
  | TEffect of typ (* 'a effect *)
  | TContinuePoint (* continue k 0 - here k type is continue point *)
  | TContinuation of
      typ (* expression 'continue k x' type is continuation ; typ = x type *)

module TVarSet = Stdlib.Set.Make (Int) (* Set, that storing type variables. *)
module VarSet = Stdlib.Set.Make (String)
(* Set, that storing variables (let-binds and effect declarations). *)

(* A schema of a type,
   consisting of a set of variables qualified for that type
   and the type itself *)
type scheme = Scheme of TVarSet.t * typ

(* Constructors for types *)
let tint = TPrim Int
let tbool = TPrim Bool
let tunit = TPrim Unit
let tchar = TPrim Char
let tstring = TPrim String
let tvar x = TVar x
let tarrow left right = TArr (left, right)
let ttuple x = TTuple x
let tlist x = TList x
let teffect x = TEffect x
let tcontinue_point = TContinuePoint
let tcontinuation expr = TContinuation expr
let ( @-> ) = tarrow
(* ---------------- *)
