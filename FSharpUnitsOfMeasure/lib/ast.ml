(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type types = 
  | FInt of int (** integer number: 0,1,2,...*)
  | FString of string (** string values: "Ocaml" *)
  | FBool of bool (** boolean values: true and false *)
  | FNil (** empty list: [] *)
  | FUnit (** ()*)
  | FFloat of float (** float number: ..., 0.1, ..., 1.2, ...*)
  | Measure_type of id (** [<Measure>] type = cm*)
  | Measure_float of types * types (** 5.0 <cm> *)

[@@deriving eq, show { with_path = false }]

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

type pattern = 
  | PWild (** _ *)
  | PConst of types (** constant pattern *)
  | PVar of id (** varuable pattern*)
  | PTuple of pattern list (** tuple pattern: (z, v) *)
  | PList of pattern list
  | PCons of pattern * pattern (** hd::tl pattern*)
[@@deriving eq, show { with_path = false }]

type expression = 
  | EConst of types
  | EVar of id
  | EBinaryOp of binary_op
  | EList of expression list
  | ETuple of expression list 
  | EApp of expression * expression
  | EIfElse of expression * expression * expression
  | ELet of id * expression
  | ELetRec of id * expression
  | EFun of pattern * expression 
  | EMatch of expression * (pattern * expression) list
[@@deriving show { with_path = false }]