(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Typedtree

val print_interpreter_error
  :  [< `Division_by_zero
     | `Handler_without_continue of string
     | `Not_continue_var of string
     | `Pattern_matching_failure
     | `Type_error
     | `Unbound_effect of string
     | `Unbound_handler of string
     | `Unbound_variable of string
     ]
  -> unit

val print_expr_value : value -> typ -> unit

val print_program_value
  :  (string, value, 'a) Base.Map.t
  -> (string, scheme, 'b) Base.Map.t
  -> string list
  -> unit
