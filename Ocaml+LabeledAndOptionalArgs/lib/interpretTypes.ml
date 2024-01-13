(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module IdMap = Map.Make (String)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VChar of char
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * enviorment
  | VLet of string * value

and enviorment = value IdMap.t [@@deriving eq]

type error =
  | UnboundVariable of string
  | ValueTypeError of value
  | ExprTypeError of string
  | DivisionByZeroError
  | ExecError of value * value
  | PatternMatchingError
[@@deriving eq]
