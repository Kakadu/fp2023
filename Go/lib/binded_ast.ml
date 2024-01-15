(** Copyright 2021-2023, Nikita Lukonenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

type typeB' = type' [@@deriving show { with_path = false }]

and signature =
  { argsB : arg list
  ; retB : Ast.ret_typ
  }
[@@deriving show { with_path = false }]

and arg = int * string * type' [@@deriving show { with_path = false }]

and ret_typ =
  | Type of typeB'
  | VoidB
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
  | MinusUn (* -expr *)
  | Not (* !expr *)
[@@deriving show { with_path = false }]

type expression =
  | ConstB of identifierB
  | IdentifierB of int * string
  | BinaryOpB of expression * binary_operation * expression
  | UnaryOpB of (unaryop * expression)
  | FunCallB of expression * expression list
  | AnonFuncB of (signature * block)
  | PrintCallB of expression list
  | LenCallB of expression
[@@deriving show { with_path = false }]

and identifierB = identifier
(* | Integer of int  (* integer identifierB *)
   | String of string  (* string identifierB *)
   | Bool of bool  (*boolean identifierB*)
   | Nil Nil identifierB *)

and var_declaration = int * string * type' * expression
and func_declaration = int * string * signature * block

and global_func_declaration = FuncTopLevelDeclarationB of func_declaration
[@@deriving show { with_path = false }]

and global_var_declaration = VarTopLevelDeclarationB of var_declaration
[@@deriving show { with_path = false }]

and block = statements list
and else_ = block

and statements =
  | IfElseStatementB of expression * block * else_
  | ForStatementB of expression * block
  | ReturnStatementB of expression option
  | BlockStatementB of block
  | ExpressinStatementB of expression
  | AssignStatementB of expression * expression
  | VarStatementB of var_declaration
  | BreakStatementB
  | ContinueStatementB
[@@deriving show { with_path = false }]
