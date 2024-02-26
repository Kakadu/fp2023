(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type ty =
  | TInt
  | TBool
  | TString
  | TList of ty
  | TArrow of ty * ty
  | TTuple of ty list
  | Unspecified
  | UserDefined of id
[@@deriving show { with_path = false }]

type ty_bind =
  { name : id
  ; ty : ty
  }
[@@deriving show { with_path = false }]

type const =
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Unit
[@@deriving show { with_path = false }]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Eq (*  = *)
  | Neq (*  != *)
  | Lt (*  < *)
  | Ltq (*  <= *)
  | Gt (*  > *)
  | Gtq (*  >= *)
[@@deriving show { with_path = false }]

type pattern =
  | PVar of id
  | PConst of const
  | PAny (* _ *)
  | PNil (* []*)
  | PTuple of pattern list
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec
  | NonRec
[@@deriving show { with_path = false }]

type expr =
  | EVar of id
  | EConst of const
  | EBinOp of binop * expr * expr (** EBinOp (op,l,r) is l op r *)
  | EFun of pattern * expr (** Fun x -> e *)
  | ECons of expr * expr (** ECons (h,t) is list h::t *)
  | ETuple of expr list (** (expr1, ..., exprn) *)
  | EIfThenElse of expr * expr * expr (** IfThenElse (b,t,e) is if b then t else e *)
  | ELet of decl * expr (** Let (x,e,e') is let x = e in e' *)
  | EApp of expr * expr (**  App (f,e) is application f e *)
  | EList of expr list
  | EMatch of expr * (pattern * expr)
  | EUnit
  | ERecord of expr list
[@@deriving show { with_path = false }]

and decl = rec_flag * id * expr

type record_bind =
  { name : id
  ; field : expr
  }
[@@deriving show { with_path = false }]
