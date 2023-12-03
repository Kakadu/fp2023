open! Base
open Types

module Constr : sig
  type t =
    | EqConstr of Ty.t * Ty.t
    | ImplInstConstr of Ty.t * VarSet.t * Ty.t
    | ExplInstConstr of Ty.t * Scheme.t

  val pp : Format.formatter -> t -> unit
end

module ConstrSet : sig
  type t

  val pp : Format.formatter -> t -> unit

  val empty : t

  val singleton : Constr.t -> t
end
