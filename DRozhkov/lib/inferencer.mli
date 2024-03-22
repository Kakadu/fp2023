(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet : sig
  module Using_comparator = Base.Set.Using_comparator

  type t = (int, Base.Int.comparator_witness) Using_comparator.t

  val empty : (int, Base.Int.comparator_witness) Base.Set.t
end

module Subst : sig
  type t = (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t

  val empty : (int, 'a, Base.Int.comparator_witness) Base.Map.t
end

module Scheme : sig
  type t = S of VarSet.t * Typedtree.typ
end

val infer_program
  :  ?env:(string, Scheme.t, Base.String.comparator_witness) Base.Map.t
  -> Ast.expression list
  -> ( (string, Scheme.t, Base.String.comparator_witness) Base.Map.t
       * (Ast.expression * Typedtree.typ) list
       , Errors.error )
       result
