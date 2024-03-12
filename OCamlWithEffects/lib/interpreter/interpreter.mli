(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Values

val run_expr_interpreter
  :  expr
  -> ( value
       , [> `Division_by_zero
         | `Handler_without_continue of id
         | `Not_continue_var of id
         | `Pattern_matching_failure
         | `Type_error
         | `Unbound_effect of id
         | `Unbound_handler of id
         | `Unbound_variable of id
         ] )
       result

val run_program_interpreter
  :  struct_item list
  -> ( (id, value, Base.String.comparator_witness) Base.Map.t
       , [> `Division_by_zero
         | `Handler_without_continue of id
         | `Not_continue_var of id
         | `Pattern_matching_failure
         | `Type_error
         | `Unbound_effect of id
         | `Unbound_handler of id
         | `Unbound_variable of id
         ] )
       result
