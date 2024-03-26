(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet : sig
  module type Hash_fold_m = Base.Hasher.S

  module Using_comparator = Base.Set.Using_comparator

  type t = (int, Base.Int.comparator_witness) Using_comparator.t

  val empty : (int, Base.Int.comparator_witness) Base.Set.t
end

type scheme = S of VarSet.t * Typedtree.typ

val run_infer
  :  ?env:(string, scheme, Base.String.comparator_witness) Base.Map.t
  -> Ast.expr list
  -> ( (string, scheme, Base.String.comparator_witness) Base.Map.t
       * (Ast.expr * Typedtree.typ) list
       , Typedtree.error )
       result
