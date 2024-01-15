(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

module Constr = struct
  module T = struct
    type unify_eff = Unify_eff | Dont_unify_eff
    [@@deriving ord, sexp_of, show {with_path= false}]

    type eff_eq = EffEq_Normal | EffEq_Late
    [@@deriving ord, sexp_of, show {with_path= false}]

    type t =
      | TyEqConstr of Ty.t * Ty.t * unify_eff
      | EffEqConstr of Eff.t * Eff.t * eff_eq
      | ExplInstConstr of Ty.t * Scheme.t
      | ImplInstConstr of Ty.t * VarSet.t * Ty.t * Eff.t
    [@@deriving ord, sexp_of, show {with_path= false}]
  end

  include T
  include Comparator.Make (T)
end

module ConstrSet = struct
  type t = (Constr.t, Constr.comparator_witness) Set.t

  let empty = Set.empty (module Constr)
  let singleton = Set.singleton (module Constr)
  let of_list = Set.of_list (module Constr)

  let is_empty = Set.is_empty

  let add = Set.add
  let union = Set.union
  let union_list = Set.union_list (module Constr)
  let remove = Set.remove
  let find_map = Set.find_map

  let fold = Set.fold
  let map = Set.map (module Constr)
end
