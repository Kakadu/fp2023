(** Copyright 2023-2024, Lesh79*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]
type decl_rec = bool [@@deriving eq, show { with_path = false }]

type bin_op =
  | Plus (* + *)
  | Minus (* - *)
  | Multi (* * *)
  | Division (* / *)
  | And (*&& *)
  | Or (* || *)
  | Equal (* = *)
  | NotEqual (* <> *)
  | Lt (* < *)
  | Ltq (* <= *)
  | Gt (* > *)
  | Gtq (* >= *)
[@@deriving eq, show { with_path = false }]

type decl_type =
  | TInt
  | TString
  | TBool
  | TFun of decl_type * decl_type
  | TList of decl_type
  | TTuple of decl_type list

let tfun dtype1 dtype2 = TFun (dtype1, dtype2)
let tlist decl_type = TList decl_type
let ttuple type_lst = TTuple type_lst

type pattern =
  | PNill
  | PString of string
  | PBool of bool
  | PInt of int
  | PVar of id
  | PTuple of pattern list
  | PFun of id * pattern
[@@deriving eq, show { with_path = false }]

let ppnill = PNill
let ppbool bool = PBool bool
let ppstring str = PString str
let ppint int = PInt int
let pptuple pattern_list = PTuple pattern_list
let ppvar id = PVar id

type expr =
  | EInt of int
  | EString of string
  | EBool of bool
  | EVar of id (* Выражения имен переменных *)
  | EBinop of bin_op * expr * expr (* Выражения бинарных операций *)
  | EIfThenElse of expr * expr * expr (* Условный оператор If then else *)
  | ETuple of expr list
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list
  | ELet of bool * id * expr * expr (* Представляет конструкцию Let*)
  | EFun of pattern * expr
[@@deriving eq, show { with_path = false }]

let e_int int = EInt int
let e_string string = EString string
let e_bool bool = EBool bool
let e_var str = EVar str
let e_binop op exp1 exp2 = EBinop (op, exp1, exp2)
let e_ifthenelse exp1 expr2 expr3 = EIfThenElse (exp1, expr2, expr3)
let e_tuple expr_list = ETuple expr_list
let e_app expr1 expr2 = EApp (expr1, expr2)
let e_match expr lst = EMatch (expr, lst)
let e_let bool id exp1 exp2 = ELet (bool, id, exp1, exp2)
let e_fun id exp = EFun (id, exp)

type decl = DeclLet of decl_rec * id * expr [@@deriving eq, show { with_path = false }]

let d_decl bool id exp = DeclLet (bool, id, exp)

type program = decl list [@@deriving eq, show { with_path = false }]
