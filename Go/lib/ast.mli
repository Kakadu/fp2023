(** Copyright 2021-2023, Nikita Lukonenko and Furetur *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* type name = string *)

(* Application [f g ] *)
(** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)

type type' =
  | StringType
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
  | One of type'
  | Void
[@@deriving show { with_path = false }]

type constant =
  | Int of int
  | String of string
  | Bool of bool
[@@deriving show { with_path = false }]

type binary_operation =
  | Minus
  | Asterisk
  | Equal
[@@deriving show { with_path = false }]

type unaryop =
  | Minus (* -expr *)
  | Not (* !expr *)
[@@deriving show { with_path = false }]

type expression =
  | Const of constant
  | Identifier of string
  | BinaryOp of expression * binary_operation * expression (* (a < b) *)
  | UnaryOp of (unaryop * expression) (* unop expr *)
  | FunCall of expression * expression list
[@@deriving show { with_path = false }]

and var_declaration = string * expression
and func_declaration = string * signature * block

and top_level_declaration =
  | FuncTopLevelDeclaration of func_declaration
  | VarTopLevelDeclaration of var_declaration
[@@deriving show { with_path = false }]

and block = statements list

and statements =
  | Ifstmt of expression * block * block
  | ReturnStatement of expression option
  | StatementsBlock of block
[@@deriving show { with_path = false }]
