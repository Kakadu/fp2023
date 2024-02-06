(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module IdMap = Map.Make (String)

type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VBool of bool
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * enviorment
  | VLet of string * bool * value

and enviorment = value IdMap.t [@@deriving eq]

type error =
  | UnboundVariable of string
  | ValueTypeError of value
  | ExprTypeError of string
  | DivisionByZero
  | ExecError of value * value
  | PatternError of string
[@@deriving eq]
