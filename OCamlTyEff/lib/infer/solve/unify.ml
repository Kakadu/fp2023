open! Base
open Types

open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

let occurs_check tv ty = VarSet.mem (Ty.vars ty) tv

let rec unify ty1 ty2 =
  match (ty1, ty2) with
  | ty1, ty2 when Ty.equal ty1 ty2 ->
      return Sub.empty
  | Ty.Ty_var tv, ty | ty, Ty.Ty_var tv ->
      if occurs_check tv ty then fail @@ TyError.OccursIn (tv, ty)
      else return @@ Sub.singleton tv ty
  | Ty_arr (l1, r1), Ty_arr (l2, r2) ->
      unify_many [l1; r1] [l2; r2]
  | Ty_tuple tys1, Ty_tuple tys2 ->
      unify_many tys1 tys2
  | _ ->
      (* XXX: constructors *)
      fail @@ UnificationFail (ty1, ty2)

(**
  Unifies types from tys1 with types
  on respective positions in tys2
*)
and unify_many tys1 tys2 =
  match (tys1, tys2) with
  | [], [] ->
      return Sub.empty
  | ty1 :: tys1, ty2 :: tys2 ->
      let* s1 = unify ty1 ty2 in
      let* s2 =
        unify_many
          (List.map tys1 ~f:(Sub.apply s1))
          (List.map tys2 ~f:(Sub.apply s1))
      in
      return @@ Sub.compose s2 s1
  | _ ->
      fail UnificationMismatch
