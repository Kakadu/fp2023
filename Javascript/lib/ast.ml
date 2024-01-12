(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type un_op =
  | Plus
  | Minus
[@@deriving show { with_path = false }]

type bin_op =
  | PropAccs
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
  | StrictEqual
  | StrictNotEqual
  | Assign
  | Rem
  | Exp
  | GreaterThan
  | GreaterEqual
  | LessThan
  | LessEqual
  | LogicalAnd
  | LogicalOr
  | BitwiseAnd
  | BitwiseOr
  | LogicalShiftLeft
  | LogicalShiftRight
  | UnsignedShiftRight
  | Xor
[@@deriving show { with_path = false }]

type typename =
  | Number of float
  | String of string
  | Bool of bool
  | Undefined
  | Null
[@@deriving show { with_path = false }]

type expression =
  | UnOp of un_op * expression
  | BinOp of bin_op * expression * expression
  | Const of typename
  | Var of string
  | Array_list of expression list
  | FunctionCall of expression * expression list
  | AnonFunction of string list * statement
  | ArrowFunction of string list * statement
  | ObjectDef of (expression * expression) list
[@@deriving show { with_path = false }]

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

and for_loop =
  { for_init : statement
  ; for_condition : statement
  ; for_change : statement
  ; for_body : statement
  }

and statement =
  | Block of statement list
  | Expression of expression
  | VarInit of var_init
  | FunInit of fun_init
  | If of expression * statement * statement
  | While of expression * statement
  | For of for_loop
  | Return of expression
  | Programm of statement list
[@@deriving show { with_path = false }]
