(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int (** 1 *)
  | Bool of bool (** true *)
  | Char of char (** 'a' *)
  | String of string (** "abc" *) (* TODO: implement parsing of {|asdada|}*)
[@@deriving eq, show { with_path = false }]

type un_op =
  | Un_plus (** + *)
  | Un_minus (** - *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
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
  | Concat (** ^ *)
  | Get_index (** s.[i] *)
[@@deriving eq, show { with_path = false }]

type val_name = LCIdent of string (** variable_name1 *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | Pat_any (** _ *)
  | Pat_val of val_name (** abc *)
  | Pat_const of const (** 1 *)
  | Pat_tuple of pattern list (** (1, 2) *)
  | Pat_cons_list of pattern * pattern (* 1::[] or [1;2] *)
  | Pat_empty_list (* []*)
[@@deriving eq, show { with_path = false }]

type expr =
  | Expr_const of const (** 1 *)
  | Un_op of un_op * expr (** -1 *)
  | Bin_op of bin_op * expr * expr (** 2 + 2 *)
  | Expr_val of val_name (** val1 *)
  | Expr_ite of expr * expr * expr (** if a then b else c *)
  | Expr_fun of val_name * expr
  (** Named function when declaring like [let inc x = x + 1] *)
  (* TODO: change val_name *)
  | Expr_app of expr * expr (** f x *)
  | Expr_let of decl * expr (** let a = 1 in b *)
  | Expr_match of expr * (pattern * expr list) (** match x with | 1 -> 0 | _ -> 1 *)
  | Expr_empty_list (** [] *)
  | Expr_cons_list of expr * expr (* 1::[] or [1;2] *)
  | Expr_tuple of expr list
[@@deriving eq, show { with_path = false }]

and decl = bool * val_name * expr

type let_decl = Let_decl of decl (** [let x = 1] *)
[@@deriving eq, show { with_path = false }]

type program = let_decl list [@@deriving eq, show { with_path = false }]
