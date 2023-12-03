open! Base

type t = (Var.t, Var.comparator_witness) Set.t

let empty = Set.empty (module Var)

let singleton = Set.singleton (module Var)

let add = Set.add

let union = Set.union

let union_list = Set.union_list (module Var)

let diff = Set.diff
