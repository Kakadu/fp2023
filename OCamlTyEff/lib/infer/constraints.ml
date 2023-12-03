open! Base
open Types

module Constr = struct
  module T = struct
    type t =
      | EqConstr of Ty.t * Ty.t
      | ImplInstConstr of Ty.t * VarSet.t * Ty.t
      | ExplInstConstr of Ty.t * Scheme.t
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
end
