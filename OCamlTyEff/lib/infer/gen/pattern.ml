(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Ast
open Constraints

open Common
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

module BoundVars = struct
  type t = (Ident.t, Var.t, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)
  let singleton = Map.singleton (module Ident)

  let idents = Map.keys
  let vars = Map.data
  let set = Map.set
  let fold = Map.fold
  let fold2 = Map.fold2

  exception Rebound of Ident.t
  let merge m1 m2 =
    try
      return
      @@ Map.merge_skewed m1 m2 ~combine:(fun ~key:id _ _ ->
             raise @@ Rebound id )
    with Rebound id -> fail @@ PatVarBoundSeveralTimes id
end

let rec helper = function
  | Pat_var id ->
      let* var = fresh_var in
      return (As.empty, BoundVars.singleton id var, ConstrSet.empty, !var)
  | Pat_any ->
      let* var = fresh_var in
      return (As.empty, BoundVars.empty, ConstrSet.empty, !var)
  | Pat_constant const ->
      return (As.empty, BoundVars.empty, ConstrSet.empty, type_of_constant const)
  | Pat_tuple pats ->
      let* asm, bound_vars, constrs, tys = helper_many pats in
      return (asm, bound_vars, constrs, Ty.Ty_tuple tys)
  | Pat_construct (con_id, con_arg) ->
      let* var_con = fresh_var in
      let as_con = As.singleton con_id var_con in
      let ty_con = !var_con in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_arg, bounds_arg, constrs =
        match con_arg with
        | None ->
            let* () = add_con_assumpt con_id NoArgs in

            return
              (As.empty, BoundVars.empty, ConstrSet.singleton (ty_con == ty_res))
        | Some con_arg ->
            let* () = add_con_assumpt con_id SomeArgs in

            let* as_arg, bounds_arg, constrs, ty_arg = helper con_arg in
            let* eff = fresh_eff in
            return
              ( as_arg
              , bounds_arg
              , ConstrSet.add constrs (ty_con == Ty_arr (ty_arg, eff, ty_res))
              )
      in

      return (as_con ++ as_arg, bounds_arg, constrs, ty_res)
  | Pat_or (pat1, pat2) ->
      let* as1, bounds1, constrs1, ty1 = helper pat1 in
      let* as2, bounds2, constrs2, ty2 = helper pat2 in

      let* sub =
        BoundVars.fold2 bounds1 bounds2 ~init:(return Sub.empty)
          ~f:(fun ~key:id ~data sub ->
            let* sub = sub in
            match data with
            | `Left _ | `Right _ ->
                fail @@ VarsMismatchOrPattern id
            | `Both (v1, v2) ->
                return @@ Sub.compose sub (Sub.singleton_ty v2 !v1) )
      in

      let constrs =
        ConstrSet.add (ConstrSet.union constrs1 constrs2) (ty1 == ty2)
      in

      return (as1 ++ as2, bounds1, Sub.apply_to_constrs sub constrs, ty1)

and helper_many pats =
  GenMonad.List.fold_right pats
    ~init:(As.empty, BoundVars.empty, ConstrSet.empty, [])
    ~f:(fun pat (acc_asm, acc_bound, acc_constrs, acc_tys) ->
      let* asm, bound, constrs, ty = helper pat in

      let* new_bounds = BoundVars.merge acc_bound bound in
      return
        ( acc_asm ++ asm
        , new_bounds
        , ConstrSet.union acc_constrs constrs
        , ty :: acc_tys ) )

let gen pat =
  let* asm, bounds, constrs, ty = helper pat in
  let* () = add_constrs_set constrs in
  return (asm, bounds, ty)

let gen_many pat =
  let* asm, bounds, constrs, tys = helper_many pat in
  let* () = add_constrs_set constrs in
  return (asm, bounds, tys)
