(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int
  | CString of string
  | CBool of bool
  | CNil
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Leq (* <= *)
  | Less (* < *)
  | Geq (* >= *)
  | Gre (* > *)
  | Eq (* == *)
  | Neq (* != *)
  | And (* && *)
  | Or (* || *)
[@@deriving eq, show { with_path = false }]

type unary_op =
  | Plus (* ~+ *)
  | Minus (* ~- *)
  | Not (* ~not *)
[@@deriving eq, show { with_path = false }]

type ident = Ident of string [@@deriving eq, show { with_path = false }]

type capitalized_ident = Capitalized_ident of string
[@@deriving eq, show { with_path = false }]

type expr =
  | EEmpty (* Empty expression *)
  | EConst of const (* Const. Examples: 100; true; "hello, world!" *)
  | EBinop of
      expr * bin_op * expr (* Binary operation. Examples: 2 + 2; (234 * 234) + 234*)
  | EUnop of unary_op * expr (* Unary operation. Examples: -(1); (+b) *)
  | EId of ident (* Identifier. Examples: a, b, c*)
  | EFun of expr (* Anonymous function. Examples: fun x -> x + 1*)
  | EApp of expr * expr (* Application. Examples: f (x - 1) *)
  | EIf of expr * expr * expr (* Conditional. Examples: if x >= y then x - y else y - x*)
  | ETuple of expr list (* Tuple. Examples: (1, 2, 3)*)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Recursive
  | Not_recursive
[@@deriving eq, show { with_path = false }]

type decl = DLet of rec_flag * ident * expr [@@deriving eq, show { with_path = false }]
type prog = decl list [@@deriving show { with_path = false }]

let econst c = EConst c
let ebinop e1 op e2 = EBinop (e1, op, e2)
let eunop op e = EUnop (op, e)
let eid i = EId i
let efun e1 = EFun e1
let eapp id args = EApp (id, args)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let etuple l = ETuple l
let dlet rf i args_e = DLet (rf, i, args_e)
let prog (d_l : decl list) : prog = d_l
