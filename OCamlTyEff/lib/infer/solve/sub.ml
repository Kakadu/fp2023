(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints

type substitute = Ty of Ty.t | Eff of Eff.t

type t = (Var.t, substitute, Var.comparator_witness) Map.t

let empty = Map.empty (module Var)
let singleton_ty var ty = Map.singleton (module Var) var (Ty ty)
let singleton_eff var eff = Map.singleton (module Var) var (Eff eff)

let mem = Map.mem

let rec apply_to_eff sub = function
  | Eff.Eff_var var as eff -> (
    match Map.find sub var with
    | Some (Eff new_eff) ->
        new_eff
    | Some (Ty _) | None ->
        eff )
  | Eff_row (Label (name, arg), eff_rest) ->
      Eff_row
        ( Label (name, Option.map arg ~f:(apply_to_ty sub))
        , apply_to_eff sub eff_rest )
  | Eff_total as eff ->
      eff

and apply_to_ty sub =
  let rec helper = function
    | Ty.Ty_var var as ty -> (
      match Map.find sub var with
      | Some (Ty new_ty) ->
          new_ty
      | Some (Eff _) | None ->
          ty )
    | Ty_arr (t1, eff, t2) ->
        Ty_arr (helper t1, apply_to_eff sub eff, helper t2)
    | Ty_con (id, tys) ->
        Ty_con (id, List.map tys ~f:helper)
    | Ty_tuple tys ->
        Ty_tuple (List.map tys ~f:helper)
  in
  helper

let apply_to_varset sub =
  (* construct new varset by adding all vars occuring in types
     on the right hand side of respective substitutions *)
  VarSet.fold ~init:VarSet.empty
    ~f:(fun acc ((Var_ty var | Var_eff var) as elt) ->
      let newvars =
        match Map.find sub var with
        | Some (Eff eff) ->
            Eff.vars eff
        | Some (Ty ty) ->
            Ty.vars ty
        | None ->
            VarSet.singleton elt
      in
      VarSet.union acc newvars )

let apply_to_scheme sub (Scheme.Forall (quantified, ty)) =
  (* don't apply substitution to quantified type variables *)
  let sub' =
    VarSet.fold quantified ~init:sub ~f:(fun acc (Var_ty var | Var_eff var) ->
        Map.remove acc var )
  in
  Scheme.Forall (quantified, apply_to_ty sub' ty)

let apply_to_env sub = Env.map ~f:(apply_to_scheme sub)

let apply_to_constrs sub =
  let apply_single : Constr.t -> Constr.t = function
    | TyEqConstr (t1, t2, flag) ->
        TyEqConstr (apply_to_ty sub t1, apply_to_ty sub t2, flag)
    | EffEqConstr (eff1, eff2, flag) ->
        EffEqConstr (apply_to_eff sub eff1, apply_to_eff sub eff2, flag)
    | ImplInstConstr (t1, mset, t2, eff2) ->
        ImplInstConstr
          ( apply_to_ty sub t1
          , apply_to_varset sub mset
          , apply_to_ty sub t2
          , apply_to_eff sub eff2 )
    | ExplInstConstr (ty, sc) ->
        ExplInstConstr (apply_to_ty sub ty, apply_to_scheme sub sc)
  in
  ConstrSet.map ~f:apply_single

let compose s1 s2 =
  Map.merge_skewed
    ~combine:(fun ~key:_ _ v2 -> v2)
    (Map.map s2 ~f:(function
      | Ty ty ->
          Ty (apply_to_ty s1 ty)
      | Eff eff ->
          Eff (apply_to_eff s1 eff) ) )
    s1
