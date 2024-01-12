(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Int of int
  | Bool of bool
  | Empty
[@@deriving show { with_path = false }]

type pattern = 
  | PAny (** _ *)
  | PConst of const (** | const -> ... *)
  | PVar of id (** | varname -> ... *)
  | PCons of pattern * pattern * pattern list (** | p1 :: p2 -> ... *)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | Neq (** != *)
  | Les (** < *)
  | Leq (** <= *)
  | Gre (** > *)
  | Geq (** >= *)
[@@deriving show { with_path = false }]

type expression =
  | Var of id
  | Const of const
  | IfThenElse of expression * expression * expression
  | Binop of bin_op * expression * expression
  | Fun of id * expression
  | Match of expression * (pattern * expression) list
  | App of expression * expression
  | Let of fun_rec * id * expression * expression option
[@@deriving show { with_path = false }]

and fun_rec =
  | Rec
  | NoRec
[@@deriving show { with_path = false }]

type program = expression list [@@deriving show { with_path = false }]