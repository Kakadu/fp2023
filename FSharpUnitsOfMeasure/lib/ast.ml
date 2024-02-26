(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type binary_op =
  | Add (** 1 + 2 *)
  | Sub (** 1 - 2 *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving eq, show { with_path = false }]

type types =
  | FInt of int (** integer number: ..., 0, 1, 2, ...*)
  | FString of string (** string values: "Ocaml" *)
  | FBool of bool (** boolean values: true and false *)
  | FNil (** [] *)
  | FUnit (** () *)
  | FFloat of float (** float number: ..., 0.1, ..., 1.2, ...*)
  | Measure_float of types * measure_type (** 5.0<cm> *)

and measure_type =
  | SMeasure of string * pow (** single measure: <m>*)
  | MMeasure of measure_type * binary_op * measure_type
  (** multiple measure: <m / sec * h ... >*)

and pow = Pow of types (** ^ *) [@@deriving eq, show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PConst of types (** constant pattern *)
  | PVar of id (** varuable pattern*)
  | PTuple of pattern list (** tuple pattern: (z, v) *)
  | PList of pattern list (** list pattern [1; 2]*)
  | PCons of pattern * pattern (** hd::tl pattern*)
[@@deriving eq, show { with_path = false }]

type measure_init =
  | SMeasure_init of measure_type (** initialization [<Measure>] type sec*)
  | MMeasure_init of measure_type * measure_type
  (** initialization [<Measure>] type speed = m/sec*)
[@@deriving show { with_path = false }]

type expression =
  | EConst of types (** constant *)
  | EVar of id (** variable *)
  | EBinaryOp of binary_op (** binary operation *)
  | EList of expression list (** list *)
  | ETuple of expression list (** tuple *)
  | EApp of expression * expression (** application *)
  | EIfElse of expression * expression * expression (** if z then v else n*)
  | ELet of string * id * expression (** let z = ... or let rec z = ...*)
  | EFun of pattern * expression (** fun z -> z + z *)
  | EMatch of expression * (pattern * expression) list (** match *)
  | EMeasure of measure_init (** measure *)
[@@deriving show { with_path = false }]
