(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type un_op =
  | Neg
  | Not
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | And (** && *)
  | Or (** || *)
  | Eq (** /= *)
  | Neq (** <> *)
  | Lt (** < *)
  | Gt (** > *)
  | Leq (** <= *)
  | Geq (** >= *)
[@@deriving eq, show { with_path = false }]

type binding = pat * expr [@@deriving eq, show { with_path = false }]

(** Alternatives in case expression *)
and alt = pat * expr [@@deriving eq, show { with_path = false }]

and expr =
  | ExprLit of lit
  | ExprVar of id
  | ExprFunc of binding (** Essentially a closure *) (* TODO: * local bindings???*)
  | ExprApp of expr * expr
  | ExprIf of expr * expr * expr
  | ExprTuple of expr list
  | ExprCons of expr * expr
  | ExprNil
  | ExprCase of expr * alt list
  | ExprBinOp of bin_op * expr * expr
  | ExprUnOp of un_op * expr
  | ExprLet of binding list * expr (** Local binding [let x = 2; y = 3 in e x y] *)
[@@deriving eq, show { with_path = false }]

and pat =
  | PatWild (** Wildcard Pat [_] *)
  | PatVar of id (** Variable Pat (name binding) [x] *)
  | PatLit of lit (** Literal pat [1], ['a'], ["hello"] *)
  | PatTuple of pat list (** Tuple pat [(x, y)] *)
  | PatCon of pat * pat (** Constructor pat *)
  | PatAs of id * pat (** As-pat [list@(x:xs)] *)
  | PatList of pat list
  | PatRec of id * pat list (**Record pat [Point {x = 3, y}] *)
  | PatNil (** [] *)
[@@deriving eq, show { with_path = false }]

and lit =
  | LitInt of int
  | LitChar of char
  | LitString of string
  | LitFloat of float
[@@deriving eq, show { with_path = false }]

and decl = DeclLet of binding [@@deriving eq, show { with_path = false }]
and prog = decl list [@@deriving eq]
