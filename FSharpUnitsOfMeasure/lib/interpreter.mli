(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Interpret_error

type value =
  | VInt of int (** int *)
  | VString of string (** string*)
  | VBool of bool (** bool *)
  | VNil (** empty list: [] *)
  | VUnit (** () *)
  | VFloat of float (** float *)
  | VTuple of value list (** tuple *)
  | VList of value list (** list *)
  | VBinOp of binary_op (** binary operation *)
  | VFun of pattern * expression * (id * value) list (** fun *)
  | VMeasureList of id list (** measure list*)
  | VFloatMeasure of value * id list (** float + measure*)
[@@deriving show { with_path = false }]

type environment = (id, value, String.comparator_witness) Map.t

val interpreter
  :  ?environment:(string, value, String.comparator_witness) Map.t
  -> expression list
  -> (environment * value, error) result
