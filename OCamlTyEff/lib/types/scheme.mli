open! Base

(** Type with universally quantified type variables *)
type t = Forall of VarSet.t * Ty.t

val free_vars : t -> VarSet.t
(** Free type variables in scheme *)
