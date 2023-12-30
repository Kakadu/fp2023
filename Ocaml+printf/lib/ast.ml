(** Copyright 2021-2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int (**  1 *)
  | Bool of bool (** true *)
[@@deriving eq, show { with_path = false }]

type un_op =
  | Un_plus (**  + *)
  | Un_minus (**  - *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** +  *)
  | Sub (** -  *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | Neq (** <> *)
  | Leq (** <= *)
  | Geq (** >= *)
  | Gre (**  > *)
  | Less (** < *)
  | And (** && *)
  | Or (** || *)
[@@deriving eq, show { with_path = false }]

type val_name = LCIdent of string (** variable_name1 *)
[@@deriving eq, show { with_path = false }]

(* not supported now (for factorail) *)
type pattern =
  | Any
  | Pat_val of val_name
  | Pat_const of const
  | Ntuple of pattern list
  | Pat_cons

type expr =
  | Expr_const of const (**         1 *)
  | Un_op of un_op * expr (**         +1 *)
  | Bin_op of bin_op * expr * expr (**       2 + 2 *)
  | Expr_val of val_name (**        val1 *)
  | ITE of expr * expr * expr (** if a then b else c *)
  | Fun of val_name * expr (** fun inc x -> x + 1 *) (* val_name will change to pattern *)
  | App of expr * expr (**        f x *)
  | Expr_let of decl * expr (**   let a = 1 in b *)
[@@deriving eq, show { with_path = false }]

and decl = bool * val_name * expr
and let_decl = Let_decl of decl (** let x = 1 *)

type program = let_decl list [@@deriving eq, show { with_path = false }]
