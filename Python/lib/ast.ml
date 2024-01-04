(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*Standart data types: integers, strings, lists*)
type value =
  | Int of int
  | String of string
  | List of value list
  | Bool of bool
  | Nil
[@@deriving show { with_path = false }]

(*Standart arithmetic operations *)
type arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
[@@deriving show { with_path = false }]

(*Funcions' name & args' name*)
type identifier = Identifier of string [@@deriving show { with_path = false }]

type modifier =
  | Global
  | Class
[@@deriving show { with_path = false }]

(*Standart boolean operators*)
type bool_op =
  | And
  | Or
  | Equal
  | NotEqual
  | GreaterOrEqual
  | Greater
  | LessOrEqual
  | Less
[@@deriving show { with_path = false }]

type f_string_type =
  | Str of value
  | Var of identifier
[@@deriving show { with_path = false }]

(*Standart expressions*)
type expression =
  | Const of value
  | Variable of modifier * identifier
  | ArithOp of arith_op * expression * expression
  | BoolOp of bool_op * expression * expression
  | FunctionCall of identifier * expression list
  | ListExp of expression list
  | Field of identifier * identifier
  | MethodCall of identifier * identifier * expression list
  | Lambda of identifier list * expression
  | FString of f_string_type list
[@@deriving show { with_path = false }]

(*Standart statements*)
type statement =
  | Expression of expression
  | Assign of expression * expression
  | Function of identifier * identifier list * statement list
  | IfElse of expression * statement list * statement list
  | Else of statement list
  | While of expression * statement list
  | Class of identifier * statement list
  | Return of expression
[@@deriving show { with_path = false }]

type flag =
  | No
  | Return_f
