(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints

type t = (Var.t, Ty.t, Var.comparator_witness) Map.t

let empty = Map.empty (module Var)
let singleton = Map.singleton (module Var)

let apply sub =
  let rec helper = function
    | Ty.Ty_var tv as ty ->
        Map.find sub tv |> Option.value ~default:ty
    | Ty_arr (t1, t2) ->
        Ty_arr (helper t1, helper t2)
    | Ty_con (id, tys) ->
        Ty_con (id, List.map tys ~f:helper)
    | Ty_tuple tys ->
        Ty_tuple (List.map tys ~f:helper)
  in
  helper

let apply_to_varset sub =
  (* construct new varset by adding all vars occuring in types
     on the right hand side of respective substitutions *)
  VarSet.fold ~init:VarSet.empty ~f:(fun acc tv ->
      let newvars =
        Map.find sub tv
        |> Option.value_map ~default:(VarSet.singleton tv) ~f:Ty.vars
      in
      VarSet.union acc newvars )

let apply_to_scheme sub (Scheme.Forall (quantified, ty)) =
  (* don't apply substitution to quantified type variables *)
  let sub' = VarSet.fold quantified ~init:sub ~f:Map.remove in
  Scheme.Forall (quantified, apply sub' ty)

let apply_to_constrs sub =
  let apply_single : Constr.t -> Constr.t = function
    | EqConstr (t1, t2) ->
        EqConstr (apply sub t1, apply sub t2)
    | ImplInstConstr (t1, mset, t2) ->
        ImplInstConstr (apply sub t1, apply_to_varset sub mset, apply sub t2)
    | ExplInstConstr (ty, sc) ->
        ExplInstConstr (apply sub ty, apply_to_scheme sub sc)
  in
  ConstrSet.map ~f:apply_single

let compose s1 s2 =
  Map.merge_skewed
    ~combine:(fun ~key:_ _ v2 -> v2)
    (Map.map s2 ~f:(apply s1))
    s1
