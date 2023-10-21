(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type const =
  | Char of char
  | Int of int
  | Bool of bool
  | Unit
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Plus
  | Dash
  | Asterisk
  | Slash
  | Eq
  | NEq
  | Gt
  | Gte
  | Lt
  | Lte
  | And
  | Or
[@@deriving eq, show { with_path = false }]

type un_op =
  | Not
  | Minus
[@@deriving eq, show { with_path = false }]

type expr =
  | Const of const
  | BinaryOperation of bin_op * expr * expr
  | UnaryOperation of un_op * expr
  | Identifier of id
  | Application of expr * expr
  | Fun of id list * expr
  | Declaration of id * id list * expr
  | RecDeclaration of id * id list * expr
[@@deriving eq, show { with_path = false }]
