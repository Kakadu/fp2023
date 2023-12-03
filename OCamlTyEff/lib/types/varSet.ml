open! Base

type t = (Var.t, Var.comparator_witness) Set.t

let compare = Set.compare_direct

let sexp_of_t = Set.sexp_of_m__t (module Var)

let pp ppf set =
  Set.to_list set |> List.map ~f:Var.show |> String.concat ~sep:", "
  |> Stdlib.Format.fprintf ppf "{%s}"

let empty = Set.empty (module Var)

let singleton = Set.singleton (module Var)

let add = Set.add

let union = Set.union

let union_list = Set.union_list (module Var)

let diff = Set.diff
