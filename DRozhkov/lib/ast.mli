(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int
  | Bool of bool

type pattern =
  | PConst of const
  | PDash
  | PVar of string

type binop =
  | Plus
  | Minus
  | Equally
  | NEqually
  | And
  | Or
  | Split
  | Mult
  | More
  | MoEq
  | Less
  | LeEq

type rec_flag =
  | Rec
  | NoRec

type expr =
  | Var of string
  | EConst of const
  | EBinop of expr * binop * expr
  | EIfThenElse of expr * expr * expr
  | ELet of rec_flag * string * expr * expr
  | Nothing
  | EFun of string * expr
  | EApp of expr * expr
  | EList of expr list
  | EMatch of expr * (pattern * expr) list

type exprs = expr list

val pp_exprs : Format.formatter -> exprs -> unit
