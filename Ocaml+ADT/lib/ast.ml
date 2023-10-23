(** Copyright 2021-2023, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binop =
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  * *)
  | Div (**  / *)
  | Eq (** == *)
  | Neq (** <> *)
  | Les (**  < *)
  | Leq (**  <= *)
  | Gre (**  > *)
  | Geq (**  >= *)
  | And (** && *)
  | Or (** || *)
  | Cons (** :: *)
[@@deriving eq, show { with_path = false }]

let bop_add = Add
let bop_sub = Sub
let bop_mul = Mul
let bop_div = Div
let bop_eq = Eq
let bop_neq = Neq
let bop_les = Les
let bop_leq = Leq
let bop_gre = Gre
let bop_geq = Geq
let bop_and = And
let bop_or = Or
let bop_cons = Cons

type decl_name =
  | LName of string (* abc, aBc *)
  | UName of string (* Number *)
[@@deriving eq, show { with_path = false }]

and decl_type =
  | DType of decl_type
  | TEmptyType
  | TInt
  | TFloat
  | TString
  | TBool
  | TFun of decl_type * decl_type
  | TVar of decl_name
  | TList of decl_type
  | TTuple of decl_type list
[@@deriving eq, show { with_path = false }]

let lname str = LName str
let uname str = UName str
let dtype decl_type = DType decl_type
let temptytype = TEmptyType
let tint = TInt
let tfloat = TFloat
let tstring = TString
let tbool = TBool
let tfun decl_type1 decl_type2 = TFun (decl_type1, decl_type2)
let tvar string = TVar string
let tlist decl_type = TList decl_type
let ttuple decl_type_list = TTuple decl_type_list

type decl_rec = DRec of bool [@@deriving eq, show { with_path = false }]

let drec bool = DRec bool

(* some decl_name's will be replaced with pattern soon *)
type pattern =
  | PNill (* [] *)
  | PString of string
  | PBool of bool
  | PInt of int
  | PFloat of float
  | PVar of decl_name
  | PWild
  | PCons of pattern * pattern
  | PTuple of pattern list
[@@deriving eq, show { with_path = false }]

type let_decl = decl_rec * decl_name * decl_type * decl_exp
[@@deriving eq, show { with_path = false }]

and type_decl = decl_name * (decl_name * decl_type) list
[@@deriving eq, show { with_path = false }]

and decl_exp =
  | EInt of int
  | EString of string
  | EBool of bool
  | EVar of decl_name
  | ETuple of decl_exp list
  | EBinop of binop * decl_exp * decl_exp
  | EFun of (decl_name * decl_type) * decl_exp
  | ELet of let_decl * decl_exp
  | EApp of decl_exp * decl_exp
  | EIf of decl_exp * decl_exp * decl_exp
[@@deriving eq, show { with_path = false }]

and decl =
  | DLet of let_decl
  | DType of type_decl
[@@deriving eq, show { with_path = false }]

let eint num = EInt num
let estring str = EString str
let ebool bool = EBool bool
let evar var = EVar var
let ebinop binop decl_exp1 decl_exp2 = EBinop (binop, decl_exp1, decl_exp2)
let efun (string, decl_type) decl_exp = EFun ((string, decl_type), decl_exp)
let elet let_decl decl_exp = ELet (let_decl, decl_exp)
let eapp decl_exp1 decl_exp2 = EApp (decl_exp1, decl_exp2)
let eif decl_exp1 decl_exp2 decl_exp3 = EIf (decl_exp1, decl_exp2, decl_exp3)
let dlet bool decl_name decl_type decl_exp = DLet (bool, decl_name, decl_type, decl_exp)

type program = decl list [@@deriving eq, show { with_path = false }]
