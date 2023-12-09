open! Base
open Types
open Ast

module Assumptions = struct
  type t = (Ident.t, Var.t list, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)

  let singleton name tv = Map.singleton (module Ident) name [tv]

  let lookup = Map.find_multi

  let remove = Map.remove

  let merge = Map.merge_skewed ~combine:(fun ~key:_ v1 v2 -> List.append v1 v2)

  let fold = Map.fold

  let idents = Map.keys
end

module ConArityAssumpt = struct
  type arity = NoArgs | SomeArgs [@@deriving eq]

  type t = (Ident.t, arity, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)

  let find = Map.find

  let set map con_id arity = Map.set map ~key:con_id ~data:arity
end
