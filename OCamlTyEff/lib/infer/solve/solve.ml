(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module Sub = Sub

open! Base
open Monads.Std
open Misc

open Types
open Constraints

module SolveMonad : sig
  include Monad.S

  val run : 'a t -> ('a, TyError.t) result

  module Solve : sig
    val fresh_var : Var.t t
    val fresh_weak : Var.t t
    val fail : TyError.t -> 'a t
  end
end = struct
  include MakeESMonad (TyError) (Int)

  let run m = run m 0 |> Result.map ~f:fst

  module Solve = struct
    let fresh_name =
      let* count = State.get in
      let* () = State.put (count + 1) in
      (* "solve" prefix is important to avoid collision
         with vars created in gen monad *)
      return ("solve" ^ Int.to_string count)

    let fresh_var = fresh_name >>| fun name -> Var.Var name
    let fresh_weak = fresh_name >>| fun name -> Var.Var_weak name

    let fail = Error.fail
  end
end

open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

module Unify = struct
  let rec rewrite_eff lbl1 = function
    | Eff.Eff_row (lbl2, eff_rest2) when Eff.Label.equal lbl1 lbl2 ->
        return (Sub.empty, eff_rest2)
    | Eff_row (lbl2, Eff.Eff_var var2) ->
        let* fresh_eff = fresh_var >>| fun var -> Eff.Eff_var var in
        return
          ( Sub.singleton_eff var2 (Eff.Eff_row (lbl1, fresh_eff))
          , Eff.Eff_row (lbl2, fresh_eff) )
    | Eff_row (lbl2, eff_rest2) ->
        let* sub, eff = rewrite_eff lbl1 eff_rest2 in
        return (sub, Eff.Eff_row (lbl2, eff))
    | _ ->
        assert false

  let rec eff_tail = function
    | Eff.Eff_total ->
        None
    | Eff_var var ->
        Some var
    | Eff_row (_, eff_rest) ->
        eff_tail eff_rest

  let occurs_check_eff var eff = VarSet.mem (Eff.vars eff) (Var_eff var)
  let rec unify_eff eff1 eff2 =
    match (eff1, eff2) with
    | eff1, eff2 when Eff.equal eff1 eff2 ->
        return Sub.empty
    | Eff.Eff_var var, eff | eff, Eff.Eff_var var ->
        if occurs_check_eff var eff then fail @@ OccursInEff (var, eff)
        else return @@ Sub.singleton_eff var eff
    | Eff_row (lbl1, eff1_rest), (Eff_row (_, _) as row2) -> (
        let* sub1, eff2_rest = rewrite_eff lbl1 row2 in

        match eff_tail eff1_rest with
        | Some tail when Sub.mem sub1 tail ->
            fail RecursiveEffRows
        | None | Some _ ->
            let* sub2 =
              unify_eff
                (Sub.apply_to_eff sub1 eff1_rest)
                (Sub.apply_to_eff sub1 eff2_rest)
            in
            return @@ Sub.compose sub2 sub1 )
    | _, _ ->
        fail @@ UnificationFailEff (eff1, eff2)

  let occurs_check_ty var ty = VarSet.mem (Ty.vars ty) (Var_ty var)
  let rec unify ty1 ty2 unify_eff_flag =
    match (ty1, ty2) with
    | ty1, ty2 when Ty.equal ty1 ty2 ->
        return Sub.empty
    | Ty.Ty_var var, ty | ty, Ty.Ty_var var ->
        if occurs_check_ty var ty then fail @@ OccursInTy (var, ty)
        else return @@ Sub.singleton_ty var ty
    | Ty_arr (l1, eff1, r1), Ty_arr (l2, eff2, r2) -> (
        let* sub1 = unify_many [l1; r1] [l2; r2] unify_eff_flag in
        match unify_eff_flag with
        | Constr.Unify_eff ->
            let* sub2 =
              unify_eff
                (Sub.apply_to_eff sub1 eff1)
                (Sub.apply_to_eff sub1 eff2)
            in
            return @@ Sub.compose sub2 sub1
        | Dont_unify_eff ->
            return sub1 )
    | Ty_tuple tys1, Ty_tuple tys2 ->
        unify_many tys1 tys2 unify_eff_flag
    | Ty_con (id1, tys1), Ty_con (id2, tys2) when Ident.equal id1 id2 ->
        unify_many tys1 tys2 unify_eff_flag
    | _ ->
        fail @@ UnificationFailTy (ty1, ty2)

  (**
    Unifies types from tys1 with types
    on respective positions in tys2
  *)
  and unify_many tys1 tys2 unify_eff_flag =
    match (tys1, tys2) with
    | [], [] ->
        return Sub.empty
    | ty1 :: tys1, ty2 :: tys2 ->
        let* s1 = unify ty1 ty2 unify_eff_flag in
        let* s2 =
          unify_many
            (List.map tys1 ~f:(Sub.apply_to_ty s1))
            (List.map tys2 ~f:(Sub.apply_to_ty s1))
            unify_eff_flag
        in
        return @@ Sub.compose s2 s1
    | _ ->
        fail UnificationMismatch
end

let generalize free ty =
  let not_weak =
    (* don't quantify weak vars *)
    VarSet.filter (Ty.vars ty) ~f:(function
      | Var_ty (Var _) | Var_eff (Var _) ->
          true
      | Var_ty (Var_weak _) | Var_eff (Var_weak _) ->
          false )
  in
  Scheme.Forall (VarSet.diff not_weak free, ty)

let weaken ty =
  let sub =
    VarSet.fold (Ty.vars ty) ~init:Sub.empty ~f:(fun acc -> function
      | Var_ty (Var name as var) ->
          Sub.compose acc @@ Sub.singleton_ty var @@ Ty_var (Var_weak name)
      | Var_eff (Var name as var) ->
          Sub.compose acc @@ Sub.singleton_eff var @@ Eff_var (Var_weak name)
      | Var_ty (Var_weak _) | Var_eff (Var_weak _) ->
          acc )
  in
  Sub.apply_to_ty sub ty

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

let activevars ?(incl_impl_inst = true) ?(incl_effeq_late = true) =
  let activevars_single = function
    | Constr.TyEqConstr (ty1, ty2, _) ->
        VarSet.union (Ty.vars ty1) (Ty.vars ty2)
    | EffEqConstr (eff1, eff2, EffEq_Normal) ->
        VarSet.union (Eff.vars eff1) (Eff.vars eff2)
    | EffEqConstr (eff1, eff2, EffEq_Late) ->
        if incl_effeq_late then VarSet.union (Eff.vars eff1) (Eff.vars eff2)
        else VarSet.empty
    | ImplInstConstr (ty1, mset, ty2, eff2) ->
        if incl_impl_inst then
          VarSet.union_list
            [Ty.vars ty1; VarSet.inter mset (Ty.vars ty2); Eff.vars eff2]
        else VarSet.empty
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
          let ok = Some (constr, rest) in

          (* enforce order in which constraints must be solved *)
          match constr with
          | TyEqConstr (_, _, Unify_eff)
          | EffEqConstr (_, _, EffEq_Normal)
          | ExplInstConstr (_, _) ->
              ok
          | TyEqConstr (t1, t2, Dont_unify_eff)
          (* solve after all other eq constraints on [t1] & [t2] are solved *)
            when VarSet.is_empty
                 @@ VarSet.inter
                      (activevars ~incl_impl_inst:false ~incl_effeq_late:false
                         rest )
                      (VarSet.union (Ty.vars t1) (Ty.vars t2)) ->
              ok
          | EffEqConstr (eff1, eff2, EffEq_Late)
          (* solve after all other constraints on [eff1] & [eff2] are solved *)
            when VarSet.is_empty
                 @@ VarSet.inter
                      (activevars ~incl_effeq_late:false rest)
                      (VarSet.union (Eff.vars eff1) (Eff.vars eff2)) ->
              ok
          | ImplInstConstr (_, mset, t2, eff2)
          (* solve after all other (excluding late EffEqs)
             constraints on [t2] & [eff2] are solved *)
            when VarSet.is_empty
                 @@ VarSet.union
                      (VarSet.inter (activevars rest)
                         (VarSet.diff (Ty.vars t2) mset) )
                      (VarSet.inter
                         (activevars ~incl_impl_inst:false
                            ~incl_effeq_late:false rest )
                         (Eff.vars eff2) ) ->
              ok
          | TyEqConstr (_, _, _)
          | EffEqConstr (_, _, _)
          | ImplInstConstr (_, _, _, _) ->
              None )
    in

    match next_solvable with
    | None ->
        (* no constraints left to solve *)
        assert (ConstrSet.is_empty cs) ;
        return Sub.empty
    | Some constr -> (
      match constr with
      | TyEqConstr (t1, t2, unify_eff_flag), cs ->
          let* s1 = Unify.unify t1 t2 unify_eff_flag in
          let* s2 = solve' (Sub.apply_to_constrs s1 cs) in
          return @@ Sub.compose s2 s1
      | EffEqConstr (eff1, eff2, _), cs ->
          let* s1 = Unify.unify_eff eff1 eff2 in
          let* s2 = solve' (Sub.apply_to_constrs s1 cs) in
          return @@ Sub.compose s2 s1
      | ImplInstConstr (t1, mset, t2, eff2), cs ->
          let sc =
            (* do not generalize if [eff2] contains [ref] label *)
            if Eff.contains eff2 (Eff.Label.ref ()) then
              Scheme.Forall (VarSet.empty, weaken t2)
            else generalize mset t2
          in
          solve' @@ ConstrSet.add cs (ExplInstConstr (t1, sc))
      | ExplInstConstr (ty, sc), cs ->
          let* ty' = instantiate sc in
          solve' @@ ConstrSet.add cs (TyEqConstr (ty, ty', Unify_eff)) )
  in
  SolveMonad.run @@ solve' cs
