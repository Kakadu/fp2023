(** Copyright 2021-2023, Kakadu, Ilya Syresenkov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*
   TODO:
   1. Implenent standart IO funtions
*)

(** Identifiers for variables and functions names *)
type id = string [@@deriving show { with_path = false }]

(** Binary operators *)
type binop =
  | Add (** + *)
  | Sub (** - *)
  | Div (** / *)
  | Mul (** * *)
  | Eq (** = *)
  | Neq (** <> *)
[@@deriving show { with_path = false }]

type const =
  | CInt of int (** Integers 1, 2, ... *)
  | CBool of bool (** Boolean true or false *)
[@@deriving show { with_path = false }]

type pattern =
  | PWild (** | _ -> ... *)
  | PConst of const (** | const -> ... *)
  | PVar of id (** | varname -> ... *)
  | PCons of pattern * pattern (** | p1 :: p2 -> ... *)
  | POr of pattern * pattern (** | p1 | p2 | p3 -> ... *)
[@@deriving show { with_path = false }]

type is_rec =
  | Rec (** Recursive functions can call themselves in their body *)
  | NonRec
[@@deriving show { with_path = false }]

type expr =
  | EUnit (** Empty expression *)
  | EConst of const (** Consts *)
  | EVar of id (** Variables with their names *)
  | EBinop of binop * expr * expr (** e1 binop e2 *)
  | ETuple of expr list (** Tuples of 2 or more elements, separeted by ',' *)
  | EList of expr list (** Lists [1; 2; 3], ...*)
  | EBranch of expr * expr * expr (** if [cond] then [a] else [b] *)
  | EMatch of expr * (pattern * expr) list (** match [x] with | [p1] -> [e1] | ... *)
  | ELet of is_rec * id * expr * expr (** let rec f *)
  | EFun of id * expr (** Anonymous function *)
  | EApp of expr * expr (** Application f x *)
[@@deriving show { with_path = false }]
