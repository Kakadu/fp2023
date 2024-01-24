(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
module VarSet = Stdlib.Set.Make (Int)
module VarMap = Stdlib.Map.Make (Int)
module StringMap = Stdlib.Map.Make (String)

type ty =
  | TVar of int
  | TPrim of string
  | TArr of ty * ty
  | TTuple of ty list
  | TList of ty
  | TAdt of string (* string arg is the name of adt type *)

let type_var x = TVar x
let tprim_int = TPrim "int"
let tprim_bool = TPrim "bool"
let tprim_string = TPrim "string"
let tarr l r = TArr (l, r)
let ( @-> ) = tarr
let tlist ty = TList ty
let ttuple ty_lst = TTuple ty_lst
let tadt str1 = TAdt str1

let rec pp_ty_tuple fmt = function
  | [] -> ()
  | [ h ] ->
    (match h with
     | TArr (_, _) -> fprintf fmt "(%a)" pp_ty h
     | _ -> fprintf fmt "%a" pp_ty h)
  | h :: tl ->
    (match h with
     | TArr (_, _) -> fprintf fmt "(%a) * %a" pp_ty h pp_ty_tuple tl
     | _ -> fprintf fmt "%a * %a" pp_ty h pp_ty_tuple tl)

and pp_ty fmt = function
  | TVar num -> fprintf fmt "'%d" num
  | TPrim str -> fprintf fmt "%s" str
  | TArr (ty1, ty2) ->
    (match ty1, ty2 with
     | TArr (_, _), _ -> fprintf fmt "(%a) -> %a" pp_ty ty1 pp_ty ty2
     | _ -> fprintf fmt "%a -> %a" pp_ty ty1 pp_ty ty2)
  | TTuple ty_lst -> fprintf fmt "%a" pp_ty_tuple ty_lst
  | TList ty1 -> fprintf fmt "%a list" pp_ty ty1
  | TAdt str1 -> fprintf fmt "%s" str1
;;

type scheme = Scheme of VarSet.t * ty

type error =
  [ `Occurs_check
  | `Unification_failed of ty * ty
  | `Wrong_exp
  | `Wrong_type
  | `Unbound_adt_type of string
  | `Unbound_variable of string
  | `Pattern_matching_failed
  ]

let pp_inf_err fmt = function
  | `Occurs_check -> fprintf fmt "Occurs_check"
  | `Unification_failed (typ1, typ2) ->
    fprintf fmt "Unification_failed: %a # %a" pp_ty typ1 pp_ty typ2
  | `Wrong_exp -> fprintf fmt "Wrong_exp"
  | `Wrong_type -> fprintf fmt "Wrong_type"
  | `Unbound_adt_type str -> fprintf fmt "Unbound_adt_type: %S" str
  | `Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
  | `Pattern_matching_failed -> fprintf fmt "Pattern_matching_failed"
;;
