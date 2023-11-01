(** Copyright 2021-2023, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type const =
  | CBool of bool (* true *)
  | CInt of int (* 42 *)
  | CString of string (* "string" *)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Mod (* % *)
  | Less (* < *)
  | LEq (**  <= *)
  | Gre (* > *)
  | GEq (**  >= *)
  | Eq (* = *)
  | NEq (* <> *)
  | And (* && *)
  | Or (* || *)
[@@deriving show { with_path = false }]

type pattern =
  | Wild (* _ *)
  | Const of const
  | Var of name
  | Tuple of pattern list (* a, b *)
  | List of pattern list (** [1;2;3] *) (* Will bite during interpreter implementation *)
[@@deriving show { with_path = false }]

type expr =
  | ConstExpr of const
  | VarExpr of name
  | ListExpr of expr list
  | TupleExpr of expr list
  | BinExpr of bin_op * expr * expr
  | IfExpr of expr * expr * expr
  | LetExpr of name * expr
  | LetRecExpr of name * expr
  | AppExpr of expr * expr
  | FunExpr of pattern * expr
[@@deriving show { with_path = false }]
