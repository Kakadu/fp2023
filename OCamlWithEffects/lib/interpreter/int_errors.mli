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
val division_by_zero : [> `Division_by_zero ]
val pattern_matching_failure : [> `Pattern_matching_failure ]
val unbound_variable : 'a -> [> `Unbound_variable of 'a ]
val unbound_effect : 'a -> [> `Unbound_effect of 'a ]
val type_error : [> `Type_error ]
val unbound_handler : 'a -> [> `Unbound_handler of 'a ]
val handler_without_continue : 'a -> [> `Handler_without_continue of 'a ]
val not_continue_var : 'a -> [> `Not_continue_var of 'a ]
