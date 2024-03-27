(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int (** int *)
  | Bool of bool (** bool *)
[@@deriving show { with_path = false }]

type pattern =
  | PConst of const (** const *)
  | PDash (** _ *)
  | PVar of string (** var -> ... *)
  | PList of pattern list (** x :: xs *)
[@@deriving show { with_path = false }]

type binop =
  | Plus (** + *)
  | Minus (** - *)
  | Equally (** == *)
  | NEqually (**<>*)
  | And (** && *)
  | Or (** || *)
  | Split (** / *)
  | Mult (** * *)
  | More (** > *)
  | MoEq (** >= *)
  | Less (** < *)
  | LeEq (** <= *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec (** recursive *)
  | NoRec (** non recursive *)
[@@deriving show { with_path = false }]

type expr =
  | Var of string (** var *)
  | EConst of const (** const *)
  | EBinop of expr * binop * expr (** a + b *)
  | EIfThenElse of expr * expr * expr (** if then else*)
  | ELet of rec_flag * string * expr * expr (**let ...*)
  | Nothing (** for in ... and else ... *)
  | EFun of string * expr (** fun *)
  | EApp of expr * expr (** f x *)
  | EList of expr list (** [a; b; c]*)
  | EMatch of expr * (pattern * expr) list (** math ... with*)
[@@deriving show { with_path = false }]

type exprs = expr list

val pp_exprs : Format.formatter -> exprs -> unit
