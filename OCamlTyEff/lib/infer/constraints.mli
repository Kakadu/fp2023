(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

module Constr : sig
  type unify_eff =
    | Unify_eff  (** Unify effects in types *)
    | Dont_unify_eff  (** Do not unify effects in types *)

  type t =
    | TyEqConstr of Ty.t * Ty.t * unify_eff
        (**
          [TyEqConstr(t1, t2, unify_eff)] reflects that t1 and t2 should be unified.
          [unify_eff] specifies if effects in t1 and t2 should be unified
        *)
    | EffEqConstr of Eff.t * Eff.t
        (** [EffEqConstr(eff1, eff2)] reflects that eff1 and eff2 should be unified *)
    | ExplInstConstr of Ty.t * Scheme.t
        (** [ExplInstConstr(ty, sc)] states that ty has to be a generic instance of sc *)
    | ImplInstConstr of Ty.t * VarSet.t * Ty.t
        (**
          [ImplInstConstr(t1, M, t2)] states that t1 should be an instance
          of the type scheme that is obtained by generalizing type t2
          with respect to the set of monomorphic type variables M
        *)
  val pp : Format.formatter -> t -> unit
end

module ConstrSet : sig
  type t

  val pp : Format.formatter -> t -> unit

  val empty : t
  val of_list : Constr.t list -> t
  val singleton : Constr.t -> t

  val add : t -> Constr.t -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val remove : t -> Constr.t -> t

  val find_map : t -> f:(Constr.t -> 'a option) -> 'a option
  (**
    Returns the first evaluation of `f` that returns `Some`,
    and returns `None` if there is no such element.
  *)

  val fold : t -> init:'acc -> f:('acc -> Constr.t -> 'acc) -> 'acc
  val map : t -> f:(Constr.t -> Constr.t) -> t
end
