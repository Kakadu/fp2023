(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Intenger of int
  | Float of float
  | Bool of bool
  | Char of char
[@@deriving show { with_path = false }]

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
[@@deriving show { with_path = false }]

type expression =
  | Empty
  | Var of id
  | Const of const
  | IfThenElse of expression * expression * expression
  | Binop of bin_op * expression * expression
  | List of expression list
  | Fun of id * expression
  | App of expression * expression
  | Let of fun_rec * id * expression * expression
[@@deriving show { with_path = false }]

and fun_rec =
  | Rec
  | NoRec
