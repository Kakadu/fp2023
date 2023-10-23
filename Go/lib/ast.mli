(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t

(* Application [f g ] *)
(** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)

type type' =
  | StringType
  | IntegerType
  | BooleanType
  | FuncType of signature

and signature =
  { args : arg list
  ; ret : ret_typ
  }

and arg = string * type'

and ret_typ =
  | One of type'
  | Void

type constant =
  | Int of int
  | String of string
  | Bool of bool

and binaryOperation =
  | Minus
  | Asterisk
  | Equal

and expression =
  | Const of constant
  | Ident of string
  | BinOp of expression * binaryOperation * expression (* (a < b) *)
  | Call of expression * expression list

and block = statements list

and statements =
  | Ifstmt of expression * block * block
  | ReturnStatement of expression option
  | StatementsBlock of block

type topLevelDeclaration =
  (* | FuncTopLevelDeclaration of string *)
  | FuncTopLevelDeclaration of string * signature * block
  | VarTopLevelDeclaration of string * expression
  | Block of block
