(** Copyright 2021-2023, Nikita Lukonenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type' =
  | StringType [@foo]
  | IntegerType
  | BooleanType
  | FuncType of signature
[@@deriving show { with_path = false }]

and signature =
  { args : arg list
  ; ret : ret_typ
  }
[@@deriving show { with_path = false }]

and arg = string * type' [@@deriving show { with_path = false }]

and ret_typ =
  | Type of type'
  | Void
[@@deriving show { with_path = false }]

type binary_operation =
  | MinusBin
  | Asterisk
  | Plus
  | Division
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | And
  | Or
[@@deriving show { with_path = false }]

type unaryop =
  | MinusUn
  | Not (* !expr *)
[@@deriving show { with_path = false }]

type expression =
  | Const of identifier
  | Identifier of string
  | BinaryOp of (expression * binary_operation * expression)
  | UnaryOp of (unaryop * expression)
  | FunCall of expression * expression list
  | AnonFunc of (signature * block)
  | PrintCall of expression list
  | LenCall of expression
[@@deriving show { with_path = false }]

and identifier =
  | Integer of int
  | String of string
  | Bool of bool
  | Nil

and var_declaration = string * type' * expression
and func_declaration = string * signature * block

and global_declaration =
  | VarTopLevelDeclaration of var_declaration
  | FuncTopLevelDeclaration of func_declaration
[@@deriving show { with_path = false }]

and block = statements list
and else_ = block

and statements =
  | IfElseStatement of expression * block * else_
  | ForStatement of expression * block
  | ReturnStatement of expression option
  | BlockStatement of block
  | ExpressinStatement of expression
  | AssignStatement of expression * expression
  | VarStatement of var_declaration
  | BreakStatement
  | ContinueStatement
[@@deriving show { with_path = false }]

type file = global_declaration list [@@deriving show { with_path = false }]
