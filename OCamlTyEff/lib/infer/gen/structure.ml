(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Ast
open Types
open Constraints

open Common
module As = Assumptions
module BoundVars = Pattern.BoundVars

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

type defined_type = {id: Ident.t; arity: int}

(**
  Check for type arity mismatches
  and unbound type variables & types
*)
let rec check_type ~types_arity ~params = function
  | Ty.Ty_var var when List.mem params var ~equal:Var.equal ->
      return ()
  | Ty_var (Var name | Var_weak name) ->
      fail @@ UnboundTypeVariable name
  | Ty_con (id, args) ->
      let* arity =
        Map.find types_arity id
        |> Option.value_map ~f:return ~default:(fail @@ UnboundType id)
      in
      if arity = List.length args then check_many ~types_arity ~params args
      else fail @@ TypeArityMismatch id
  | Ty_arr (l, _, r) ->
      let* () = check_type ~types_arity ~params l in
      check_type ~types_arity ~params r
  | Ty_tuple tys ->
      check_many ~types_arity ~params tys

and check_many ~types_arity ~params =
  GenMonad.List.iter ~f:(check_type ~types_arity ~params)

let gen types_arity = function
  | Str_eval e ->
      let* as_e, ty_e, eff_e = Expr.gen e in
      return (as_e, BoundVars.empty, Some ty_e, eff_e, [])
  | Str_type {id= defined_type_id; params; variants} ->
      let defined_type =
        Ty.Ty_con (defined_type_id, List.map params ~f:( ! ))
      in
      let defined_type_arity = List.length params in

      let types_arity =
        Map.set types_arity ~key:defined_type_id ~data:defined_type_arity
      in
      let* bounds =
        GenMonad.List.fold variants ~init:BoundVars.empty
          ~f:(fun acc {id= con_id; arg} ->
            let* var_con = fresh_var in

            let* () =
              match arg with
              | None ->
                  add_constrs [!var_con == defined_type]
              | Some arg ->
                  let* () = check_type ~types_arity ~params arg in
                  let* eff = fresh_eff in
                  add_constrs [!var_con == Ty_arr (arg, eff, defined_type)]
            in

            return @@ BoundVars.set acc ~key:con_id ~data:var_con )
      in

      let* eff = fresh_eff in
      return
        ( As.empty
        , bounds
        , None
        , eff
        , [{id= defined_type_id; arity= defined_type_arity}] )
  | Str_exception {id= Ident name as id; arg} ->
      let exn_type = Ty.exn (Ty_con (Ident ("_" ^ name), [])) in

      let* var_exn = fresh_var in
      let* () =
        match arg with
        | None ->
            add_constrs [!var_exn == exn_type]
        | Some arg ->
            let* () = check_type ~types_arity ~params:[] arg in
            let* eff = fresh_eff in
            add_constrs [!var_exn == Ty_arr (arg, eff, exn_type)]
      in

      let* eff = fresh_eff in
      return (As.empty, BoundVars.singleton id var_exn, None, eff, [])
  | Str_value (Nonrecursive, bindings) ->
      let* pat, e =
        match bindings with
        | [{pat; expr}] ->
            return (pat, expr)
        | _ ->
            fail @@ NotImplemented "several value bindings (and)"
      in

      let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
      let* as_e, ty_e, eff_e = Expr.gen e in

      let* () = add_constrs [ty_pat == ty_e] in

      return (as_pat ++ as_e, bounds_pat, None, eff_e, [])
  | Str_value (Recursive, bindings) ->
      let* id, e =
        match bindings with
        | [{pat; expr}] -> (
          match pat with
          | Pat_var id ->
              return (id, expr)
          | _ ->
              fail NotVarLHSRec )
        | _ ->
            fail @@ NotImplemented "mutually recursive bindings"
      in
      let* () = check_rec_rhs id e in

      let* as_e, ty_e, eff_e = Expr.gen e in

      let* () =
        add_constrs
          ( As.lookup as_e id
          |> List.map ~f:(fun var_expr ->
                 (* must not unify effects in `!var_expr` & `ty_e`
                    to ensure proper type of `id` *)
                 Constr.TyEqConstr (!var_expr, ty_e, Dont_unify_eff) ) )
      in

      let* var_id = fresh_var in
      let* () = add_constrs [!var_id == ty_e] in

      return (as_e -- [id], BoundVars.singleton id var_id, None, eff_e, [])
