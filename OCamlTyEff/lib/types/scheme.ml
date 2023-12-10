open! Base

type t = Forall of VarSet.t * Ty.t [@@deriving ord, sexp_of]

let pp ppf (Forall (quantified, ty)) =
  let open Stdlib.Format in
  if VarSet.is_empty quantified then fprintf ppf "%a" Ty.pp ty
  else fprintf ppf "%a. %a" VarSet.pp quantified Ty.pp ty

let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
