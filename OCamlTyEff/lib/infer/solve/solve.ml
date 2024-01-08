(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module Sub = Sub

open! Base
open Monads.Std
open Utils

open Ast
open Types
open Constraints

module SolveMonad : sig
  include Monad.S

  val run : 'a t -> ('a, TyError.t) result

  module Solve : sig
    val fresh_var : Var.t t
    val fail : TyError.t -> 'a t
  end
end = struct
  include MakeSEMonad (Int) (TyError)

  let run m = run m 0 |> Result.map ~f:fst

  module Solve = struct
    let fresh_var =
      let* count = State.get in
      let* () = State.put (count + 1) in
      (* "solve" prefix is important to avoid collision
         with vars created in gen monad *)
      return @@ Var.Var ("solve" ^ Int.to_string count)

    let fail = Error.fail
  end
end

open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

module Unify = struct
  let occurs_check_ty var ty = VarSet.mem (Ty.vars ty) (Var_ty var)

  let rec unify ty1 ty2 =
    match (ty1, ty2) with
    | ty1, ty2 when Ty.equal ty1 ty2 ->
        return Sub.empty
    | Ty.Ty_var var, ty | ty, Ty.Ty_var var ->
        if occurs_check_ty var ty then fail @@ TyError.OccursIn (var, ty)
        else return @@ Sub.singleton_ty var ty
    | Ty_arr (l1, _, r1), Ty_arr (l2, _, r2) ->
        unify_many [l1; r1] [l2; r2]
    | Ty_tuple tys1, Ty_tuple tys2 ->
        unify_many tys1 tys2
    | Ty_con (id1, tys1), Ty_con (id2, tys2) when Ident.equal id1 id2 ->
        unify_many tys1 tys2
    | _ ->
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
            (List.map tys1 ~f:(Sub.apply_to_ty s1))
            (List.map tys2 ~f:(Sub.apply_to_ty s1))
        in
        return @@ Sub.compose s2 s1
    | _ ->
        fail UnificationMismatch
end

let generalize free ty = Scheme.Forall (VarSet.diff (Ty.vars ty) free, ty)

let instantiate (Scheme.Forall (quantified, ty)) =
  (* construct substitution that maps quantified vars to fresh vars *)
  let* subst =
    VarSet.fold quantified ~init:(return Sub.empty) ~f:(fun acc elt ->
        let* fresh = fresh_var in
        let single =
          match elt with
          | Var_ty var ->
              Sub.singleton_ty var (Ty_var fresh)
          | Var_eff var ->
              Sub.singleton_eff var (Eff_var fresh)
        in
        let* acc = acc in
        return @@ Sub.compose single acc )
  in
  return @@ Sub.apply_to_ty subst ty

let activevars =
  let activevars_single = function
    | Constr.TyEqConstr (ty1, ty2) ->
        VarSet.union (Ty.vars ty1) (Ty.vars ty2)
    | EffEqConstr (eff1, eff2) ->
        VarSet.union (Eff.vars eff1) (Eff.vars eff2)
    | ImplInstConstr (ty1, mset, ty2) ->
        VarSet.union (Ty.vars ty1) (VarSet.inter mset (Ty.vars ty2))
    | ExplInstConstr (ty, sc) ->
        VarSet.union (Ty.vars ty) (Scheme.free_vars sc)
  in
  ConstrSet.fold ~init:VarSet.empty ~f:(fun acc constr ->
      VarSet.union acc (activevars_single constr) )

let solve cs =
  let rec solve' cs =
    let next_solvable =
      ConstrSet.find_map cs ~f:(fun constr ->
          let rest = ConstrSet.remove cs constr in
          match constr with
          | TyEqConstr (_, _) | EffEqConstr (_, _) | ExplInstConstr (_, _) ->
              Some (constr, rest)
          | ImplInstConstr (_, mset, t2)
            when VarSet.is_empty
                 @@ VarSet.inter (activevars rest)
                      (VarSet.diff (Ty.vars t2) mset) ->
              Some (constr, rest)
          | ImplInstConstr (_, _, _) ->
              None )
    in
    match next_solvable with
    | None ->
        (* no constraints left to solve *)
        return Sub.empty
    | Some constr -> (
      match constr with
      | TyEqConstr (t1, t2), cs ->
          let* s1 = Unify.unify t1 t2 in
          let* s2 = solve' (Sub.apply_to_constrs s1 cs) in
          return @@ Sub.compose s2 s1
      | EffEqConstr (_, _), _ ->
          return @@ Sub.empty
      | ImplInstConstr (t1, mset, t2), cs ->
          solve' @@ ConstrSet.add cs (ExplInstConstr (t1, generalize mset t2))
      | ExplInstConstr (ty, sc), cs ->
          let* ty' = instantiate sc in
          solve' @@ ConstrSet.add cs (TyEqConstr (ty, ty')) )
  in
  SolveMonad.run @@ solve' cs
