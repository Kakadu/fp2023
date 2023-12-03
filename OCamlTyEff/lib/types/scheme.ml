open! Base

type t = Forall of VarSet.t * Ty.t

let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
