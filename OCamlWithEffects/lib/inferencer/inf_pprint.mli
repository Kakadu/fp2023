(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

val print_inferencer_error
  :  [< `Handler_without_continue
     | `Not_continue_val of string
     | `Handler_without_effect
     | `Not_effect_with_args of string
     | `Not_effect_without_args of string
     | `Occurs_check
     | `Perform_with_no_effect
     | `Several_bounds of string
     | `Unbound_effect of string
     | `Unbound_variable of string
     | `Unification_failed of typ * typ
     | `Wrong_effect_typ of string * typ
     ]
  -> unit

val expr_without_name : typ -> string
val expr_with_name : string -> typ -> string
val print_expr_type : typ -> unit
val print_program_type : (string, scheme, 'a) Base.Map.t -> string list -> unit
