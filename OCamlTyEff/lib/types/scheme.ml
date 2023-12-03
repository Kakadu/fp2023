open! Base

type t = Forall of VarSet.t * Ty.t
[@@deriving ord, sexp_of, show {with_path= false}]

let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
