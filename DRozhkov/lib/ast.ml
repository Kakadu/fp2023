(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Int of int (** int *)
  | Bool of bool (** bool *)
  | Empty (** () *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** _ *)
  | PConst of const (** const *)
  | PVar of id (** string *)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Mul (** [*] *)
  | Div (** [/] *)
  | Eq (** [=] *)
  | Neq (** [!=] *)
  | Les (** [<] *)
  | Leq (** [<=] *)
  | Gre (** [>] *)
  | Geq (** [>=] *)
[@@deriving show { with_path = false }]

type fun_rec =
  | Rec (** let rec f *)
  | NoRec (** let f *)
[@@deriving show { with_path = false }]

type expression =
  | Nothing (** nothing in let *)
  | Var of id (** string *)
  | Const of const (** const *)
  | IfThenElse of expression * expression * expression (** if ... then ... else ... *)
  | Binop of bin_op * expression * expression (** 1 + 2 *)
  | Fun of id * expression (** fun *)
  | List of expression list (** [1; 2; 3]*)
  | Match of expression * (pattern * expression) list (** match *)
  | App of expression * expression (** f x *)
  | Let of fun_rec * id * expression * expression (** let ...*)
[@@deriving show { with_path = false }]

type expr_list = expression list [@@deriving show { with_path = false }]
