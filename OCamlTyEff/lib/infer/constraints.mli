open! Base
open Types

module Constr : sig
  type t =
    | EqConstr of Ty.t * Ty.t
        (** [EqConstr(t1, t2)] reflects that t1 and t2 should be unified *)
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

  val union : t -> t -> t

  val union_list : t list -> t

  val add : t -> Constr.t -> t

  val remove : t -> Constr.t -> t

  val find_map : t -> f:(Constr.t -> 'a option) -> 'a option
  (**
    Returns the first evaluation of `f` that returns `Some`,
    and returns `None` if there is no such element.
  *)

  val fold : t -> init:'acc -> f:('acc -> Constr.t -> 'acc) -> 'acc

  val map : t -> f:(Constr.t -> Constr.t) -> t
end
