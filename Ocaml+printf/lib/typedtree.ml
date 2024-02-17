(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TVar of int (** type var *)
  | TPrim of string (** ground type *)
  | TArr of typ * typ (** function type *)
  | TUnit (** unit *)
  | TTuple of typ * typ list (** tuple type *)
  | TList of typ (** list type *)
  | TFString of typ (** example: "%d%s" has type TFString (int -> char -> unit) *)

module TypeVarSet = Stdlib.Set.Make (Int)

type scheme = Scheme of TypeVarSet.t * typ

let type_var x = TVar x
let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let char_typ = TPrim "char"
let string_typ = TPrim "string"
let unit_typ = TUnit
let format_typ x = TFString x
let arrow l r = TArr (l, r)
let ( @-> ) = arrow
