(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type types = 
  | FInt of int (** integer number: 0,1,2,...*)
  | FString of string (** string values: "Ocaml" *)
  | FBool of bool (** boolean values: true and false *)
  | FNil (** empty list: [] *)
  | FUnit (** ()*)
  | FFloat of float (** float number: ..., 0.1, ..., 1.2, ...*)
[@@deriving show { with_path = false }]

type binary_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Less (* < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving show { with_path = false }]

type expression = 
  | ETypes of types
  | EVar of id
  | EBinaryOp of binary_op
  | EList of expression * expression
  | ETuple of expression list 
  | EApp of expression * expression
  | EIfElse of expression * expression * expression
  | ELet of id * expression
  | ELetRec of id * expression
  | EFun of expression * expression 
[@@deriving show { with_path = false }]