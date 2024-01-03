(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
[@@deriving eq, show {with_path = false}]

type typename =
  | Number of float
  | String of string
  | VarType
  | FuncType
[@@deriving eq, show {with_path = false}]

type expression =
  | BinOp of bin_op * expression * expression
  | UnrecognizedOp of bin_op
  | Parens of expression
  | Const of typename
  | Var of string
  | FunctionCall of string * expression list
[@@deriving eq, show {with_path = false}]

type var_init =
{
  var_identifier: string;
  is_const: bool;
  var_type: typename;
  value: expression option
}

and fun_init =
{
  fun_identifier: string;
  arguments: expression list;
  body: statement
}

and statement =
  | Block of statement list
  | Expression of expression
  | VarDeck of var_init
  | FunDeck of fun_init
  | If of expression * statement * statement
  | Return of expression
  | Programm of statement list
[@@deriving eq, show { with_path = false }]
