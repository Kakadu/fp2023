(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser *)

type id = string [@@deriving eq, show { with_path = false }]

type un_op =
  | Plus (* unary operator +, example: +1 or 1*)
  | Minus (* unary operator -, example: -1*)
[@@deriving eq, show { with_path = false }]

type types = 
  | FInt of un_op * int (** integer number: ..., 0, 1, 2, ...*)
  | FString of string (** string values: "Ocaml" *)
  | FBool of bool (** boolean values: true and false *)
  | FNil (** empty list: [] *)
  | FUnit (** () *)
  | FFloat of un_op * float (** float number: ..., 0.1, ..., 1.2, ...*)
  | Measure_float of types * measure_type (** 5.0 <cm> *) 

and measure_init = 
  | Measure_init of measure_type (** initialization [<Measure>] type sec*)
  | Measure_multiple_init of measure_type * measure_type (** initialization [<Measure>] type speed = m/sec*)

and measure_type =
  | Measure_single of string (** single measure: <m>*)
  | Measure_multiple of measure_type * binary_op * measure_type (** double measure: <m/sec>*)
[@@deriving eq, show { with_path = false }]

and binary_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
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
  | PList of pattern list (** list pattern [1; 2]*)
  | PCons of pattern * pattern (** hd::tl pattern*)
[@@deriving eq, show { with_path = false }]

type expression = 
  | EConst of types  (* constant *)
  | EVar of id (* variable *)
  | EBinaryOp of binary_op (* binary operation *)
  | EList of expression list (* list *)
  | ETuple of expression list  (* tuple *)
  | EApp of expression * expression (* application *) 
  | EIfElse of expression * expression * expression (* if z then v else n*)
  | ELet of string * id * expression (* let z = ... or let rec z = ...*) 
  | EFun of pattern * expression  (* fun z -> z + z *)
  | EMatch of expression * (pattern * expression) list (* match *)
  | EMeasure of measure_init (* measure *)
[@@deriving show { with_path = false }]

(* interpreter *)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VNil
  | VUnit
  | VFloat of float 
  | VTuple of value list
  | VList of value list
  | VBinOp of binary_op
  | VFun of pattern * expression * (id * value) list
  | VMeasureList of (id * id list) list
  | VFloatMeasure of value * id list
[@@deriving show { with_path = false }]