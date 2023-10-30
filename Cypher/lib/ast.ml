(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

type name = string [@@deriving show { with_path = false }]

type literal =
  | True
  | False
  | Null
[@@deriving show { with_path = false }]

type constant =
  | Float of float
  | Int64 of int64
  | String of string
[@@deriving show { with_path = false }]

type bin_op =
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Percent
  | Caret
  | AND
  | OR
  | XOR
[@@deriving show { with_path = false }]

type un_op =
  | Minus
  | IS_NULL
  | IS_NOT_NULL
  | NOT
[@@deriving show { with_path = false }]

type list_op =
  | Eq
  | NEq
  | Less
  | Greater
  | LEq
  | GEq
[@@deriving show { with_path = false }]

type expression =
  | Liter of literal
  | Const of constant
  | Var of name
  | Property of name * name
  | Bin_op of bin_op * expression * expression
  | Un_op of un_op * expression
  | List_op of expression * (list_op * expression) list
[@@deriving show { with_path = false }]
