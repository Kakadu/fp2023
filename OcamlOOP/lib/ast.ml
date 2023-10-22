(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Id of string [@@deriving eq, show { with_path = false }]

let p_id s = Id s

type private_flag =
  | Private
  | Public
[@@deriving eq, show { with_path = false }]

type closed_flag =
  | Closed
  | Open
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Asterisk
  | Divider
  | Plus
  | Sub
  | Eq
  | Neq
  | Lt
  | Ltq
  | Gt
  | Gtq
  | And
  | Or
[@@deriving eq, show { with_path = false }]

type unary_op =
  | Minus
  | Not
[@@deriving eq, show { with_path = false }]

type const =
  | Int of int
  | Bool of bool
  | Nil
  | Unit
[@@deriving eq, show { with_path = false }]

type ptrn =
  | PConst of const
  | PVal of ident
  | Pcons of ptrn * ptrn
  | Pany (* the pattern _ *)
[@@deriving eq, show { with_path = false }]

type exp =
  | EConst of const
  | UnaryOp of unary_op * exp
  | BinOp of bin_op * exp * exp
  | EVal of ident
  | Fun of ptrn * exp
  | Let of decl * exp
  | LetRec of decl * exp
  | Match of exp * (ptrn * exp) list
  | IfThenElse of exp * exp * exp
  | App of exp * exp
  | Eobject of ptrn * field list
  | Esend of exp * ptrn (* not sure *)
[@@deriving eq, show { with_path = false }]

and decl = Pdecl of ptrn * exp

and field =
  | Oval of ptrn * exp
  | Omethod of private_flag * ptrn * exp

let c_int n = Int n
let c_bool b = Bool b
let nil = Nil
let econst c = EConst c
let pconst c = PConst c
let pcons a b = Pcons (a, b)
let pany = Pany
let pnil = PConst Nil
let eval c = EVal c
let pval c = PVal c
let un_op o e = UnaryOp (o, e)
let bin_op o l r = BinOp (o, l, r)
let pdecl i e = Pdecl (i, e)
let plet d e = Let (d, e)
let prlet d e = LetRec (d, e)
let pfun i e = Fun (i, e)
let ematch v ptrns = Match (v, ptrns)
let eapp f a = App (f, a)
let ite b t e = IfThenElse (b, t, e)
let oval p e = Oval (p, e)
let omthd f p e = Omethod (f, p, e)
let eobj s flds = Eobject (s, flds)
let esend s m = Esend (s, m)
