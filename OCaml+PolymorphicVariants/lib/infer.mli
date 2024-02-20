(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `No_variable of string
  | `Occurs_check
  | `Unexpected_expression
  | `Unexpected_pattern
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit

val run_infer
  :  Ast.structure
  -> ((string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t, error) result

val test_infer : string -> unit
