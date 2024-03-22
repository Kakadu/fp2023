(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type typ =
  | TInt (* Type: Intenger*)
  | TBool (* Type: Bool*)
  | TEmpty (* Type: ()*)
  | TVar of int (* Type: var *)
  | TList of typ (* Type: list *)
  | TArrow of typ * typ (* Type: function *)

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TEmpty -> fprintf fmt "empty"
  | TVar x -> fprintf fmt "'%d" x
  | TList x -> fprintf fmt "%a list" pp_typ x
  | TArrow (l, r) ->
    (match l, r with
     | TArrow _, _ -> fprintf fmt "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf fmt "%a -> %a" pp_typ l pp_typ r)
;;
