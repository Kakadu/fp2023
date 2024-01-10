(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module Var = struct
  module T = struct
    type t = Var of string [@@deriving eq, ord, sexp_of]

    let pp ppf (Var name) = Stdlib.Format.fprintf ppf "'%s" name
  end

  include T
  include Comparator.Make (T)
end

module VarSet = struct
  module Elt = struct
    module T = struct
      type t = Var_ty of Var.t | Var_eff of Var.t
      [@@deriving eq, ord, sexp_of]

      let pp ppf (Var_ty var | Var_eff var) = Var.pp ppf var
    end

    include T
    include Comparator.Make (T)
  end

  type t = (Elt.t, Elt.comparator_witness) Set.t

  let pp ppf set =
    let open Stdlib.Format in
    let vars = Set.to_list set in
    pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Elt.pp ppf vars

  let compare = Set.compare_direct
  let sexp_of_t set = Set.sexp_of_m__t (module Elt) set

  let empty = Set.empty (module Elt)
  let singleton = Set.singleton (module Elt)
  let singleton_ty var = Set.singleton (module Elt) (Var_ty var)
  let singleton_eff var = Set.singleton (module Elt) (Var_eff var)
  let of_list = Set.of_list (module Elt)
  let to_list = Set.to_list

  let is_empty = Set.is_empty
  let mem = Set.mem

  let add = Set.add
  let union = Set.union
  let union_list = Set.union_list (module Elt)
  let inter = Set.inter
  let diff = Set.diff

  let fold = Set.fold
  let fold_right = Set.fold_right
end
