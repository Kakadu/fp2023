(** Copyright 2023-2024, Danil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | OccursCheck
  | NoVariable of string
  | UnificationFailed of Typedtree.ty * Typedtree.ty

val pp_error : Format.formatter -> error -> unit

val run_prog
  :  Ast.decl list
  -> ((string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t, error) result

val pp_program : Format.formatter -> (string, Typedtree.scheme, 'a) Base.Map.t -> unit
val infer : Ast.decl list -> unit
