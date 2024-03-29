(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type typ =
  | TInt (** int type *)
  | TBool (** bools type *)
  | TArrow of typ * typ (** type -> type *)
  | TVar of int (** var type *)
  | TList of typ (* list type *)

let rec pp_typ fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TVar x -> fprintf fmt "'%d" x
  | TList x -> fprintf fmt "%a list" pp_typ x
  | TArrow (l, r) ->
    (match l, r with
     | TArrow _, _ -> fprintf fmt "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf fmt "%a -> %a" pp_typ l pp_typ r)
;;

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ

let pp_error ppf : error -> _ = function
  | Occurs_check -> fprintf ppf "Occurs check failed"
  | No_variable s -> fprintf ppf "Undefined variable '%s'" s
  | Unification_failed (l, r) ->
    Format.fprintf ppf "Unification fail on %a and %a" pp_typ l pp_typ r
;;
