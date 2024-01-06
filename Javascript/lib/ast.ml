(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type un_op =
  | Plus
  | Minus
[@@deriving eq, show { with_path = false }]

type bin_op =
  | PropAccs
  | SqPropAccs
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
  | Assign
[@@deriving eq, show { with_path = false }]

type typename =
  | Number of float
  | String of string
  | Bool of bool
  | Undefined
  | Null
[@@deriving eq, show { with_path = false }]

type expression =
  | UnOp of un_op * expression
  | BinOp of bin_op * expression * expression
  | Const of typename
  | Var of string
  | FunctionCall of expression * expression list
  | AnonFunction of string list * statement
  | ObjectDef of (expression * expression) list
[@@deriving eq, show { with_path = false }]

and var_init =
  { var_identifier : string
  ; is_const : bool
  ; value : expression
  }

and fun_init =
  { fun_identifier : string
  ; arguments : string list
  ; body : statement
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
