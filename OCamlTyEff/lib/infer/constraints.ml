(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

module Constr = struct
  module T = struct
    type t =
      | EqConstr of Ty.t * Ty.t
      | ExplInstConstr of Ty.t * Scheme.t
      | ImplInstConstr of Ty.t * VarSet.t * Ty.t
    [@@deriving ord, sexp_of, show {with_path= false}]
  end

  include T
  include Comparator.Make (T)
end

module ConstrSet = struct
  type t = (Constr.t, Constr.comparator_witness) Set.t

  let pp ppf set =
    Set.to_list set |> List.map ~f:Constr.show |> String.concat ~sep:", "
    |> Stdlib.Format.fprintf ppf "{%s}"

  let empty = Set.empty (module Constr)
  let singleton = Set.singleton (module Constr)
  let of_list = Set.of_list (module Constr)

  let add = Set.add
  let union = Set.union
  let union_list = Set.union_list (module Constr)
  let remove = Set.remove
  let find_map = Set.find_map

  let fold = Set.fold
  let map = Set.map (module Constr)
end
