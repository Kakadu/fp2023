(** Copyright 2023-2024, Danil P*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val run_prog
  :  Ast.decl list
  -> ( (string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t
       , Typedtree.error )
       result

val pp_program : Format.formatter -> (string, Typedtree.scheme, 'a) Base.Map.t -> unit
val infer : Ast.decl list -> unit
