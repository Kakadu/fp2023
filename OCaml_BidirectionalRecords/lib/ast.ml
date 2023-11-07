(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show]

type ty =
  | TInt
  | TBool
  | TString
  | TList of ty
  | TTuple of ty list
  | Unspecified
  | UserDefined of id
[@@deriving show]

type ty_bind =
  { name : id
  ; ty : ty
  }
[@@deriving show]

type const =
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Nil
  | Unit
[@@deriving show]

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
[@@deriving show]

type unop =
  | UNot (** not a *)
  | UMinus (*  -5 *)
  | UPlus
[@@deriving show]

type pattern =
  | PVar of ty_bind
  | PConst of const
  | PCons of pattern * pattern list (** PCons (p1,p2) is p1::p2 *)
  | PAny (* _ *)
  | PTuple of pattern list
[@@deriving show]

type rec_flag =
  | Rec
  | NonRec
[@@deriving show]

type expr =
  | EVar of ty_bind
  | EConst of const
  | EBinOp of binop * expr * expr (** EBinOp (op,l,r) is l op r *)
  | EUnOp of unop * expr (** EUnOp (op,e) is op e *)
  | EFun of ty_bind * expr (** Fun x -> e *)
  | ECons of expr * expr (** ECons (h,t) is list h::t *)
  | ETuple of expr list (** (expr1, ..., exprn) *)
  | EIfThenElse of expr * expr * expr (** IfThenElse (b,t,e) is if b then t else e *)
  | ELet of rec_flag * ty_bind * expr * expr (** Let (x,e,e') is let x = e in e' *)
  | EApp of expr * expr (**  App (f,e) is application f e *)
  | EMatch of expr * (pattern * expr)
[@@deriving show]

type record = ty_bind list [@@deriving show]
