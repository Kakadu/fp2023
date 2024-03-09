(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree
open Inf_errors

val run_expr_inferencer : expr -> (typ, error) result

val run_program_inferencer
  :  struct_item list
  -> ((id, scheme, Base.String.comparator_witness) Base.Map.t * id list, error) result
