(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `No_variable of string
  | `Occurs_check
  | `Pattern_matching_error
  | `Not_implemented
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type t = Set.Make(Base.Int).t
end

type scheme = S of VarSet.t * Typedtree.ty

val run_infer
  :  Ast.structure
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

val test_infer : string -> unit
