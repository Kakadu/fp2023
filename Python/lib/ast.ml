(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*Standart data types: integers, strings, lists*)
type value =
  | Int of int
  | String of string
  | List of value list
[@@deriving eq, show { with_path = false }]

(*Standart arithmetic operations *)
type arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
[@@deriving eq, show { with_path = false }]

(*Funcions' name & args' name*)
type identifier = Identifier of string [@@deriving eq, show { with_path = false }]

(*Standart boolean operators*)
type bool_op =
  | And
  | Or
  | Equal
[@@deriving eq, show { with_path = false }]

(*Standart expressions*)
type expression =
  | Const of value
  | Variable of identifier
  | ArithOp of arith_op * expression * expression
  | BoolOp of bool_op * expression * expression
  | FunctionCall of identifier * expression list
[@@deriving eq, show { with_path = false }]  

type statement =
  | Expression of expression
  | IfElse of expression * statement list * statement list
  | While of expression * statement list
  | Assign of expression * expression
  | Return of expression
  | Function of identifier * identifier list * statement list
[@@deriving eq, show { with_path = false }]
