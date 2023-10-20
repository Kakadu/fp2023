(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Devision_by_zero
  | `Pattern_matching_failure
  | `Unbound_variable of id (* Unreachable after type checking *)
  | `Unbound_effect of id (* Unreachable after type checking *)
  | `Unbound_handler of id
  | `Type_error (* Unreachable after type checking *)
  | `Handler_without_continue of id (* Unreachable after type checking *)
  | `Not_continue_var of id (* Unreachable after type checking *)
  ]
(* More detailed descriptions of errors can be found in the file int_pprint.ml. *)

(* Constructors for types *)
let division_by_zero = `Division_by_zero
let pattern_matching_failure = `Pattern_matching_failure
let unbound_variable name = `Unbound_variable name
let unbound_effect name = `Unbound_effect name
let type_error = `Type_error
let unbound_handler name = `Unbound_handler name
let handler_without_continue name = `Handler_without_continue name
let not_continue_var name = `Not_continue_var name
(* ---------------- *)
