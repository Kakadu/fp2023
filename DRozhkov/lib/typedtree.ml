(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type typ =
  | TInt
  | TBool
  | TEmpty
  | TVar of int
  | TList of typ
  | TArrow of typ * typ
[@@deriving eq, show { with_path = false }]

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TEmpty -> fprintf fmt "empty"
  | TVar x -> fprintf fmt "'%d" x
  | TList x -> fprintf fmt "%a list" pp_typ x
  | TArrow (l, r) ->
    (match l, r with
     | TArrow (_, _), _ -> fprintf fmt "(%a) -> %a" pp_typ l pp_typ r
     | _, _ -> fprintf fmt "%a -> %a" pp_typ l pp_typ r)
;;