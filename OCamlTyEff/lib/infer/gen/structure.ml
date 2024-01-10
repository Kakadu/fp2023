(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Types
open Constraints

open Common
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

let gen = function
  | Str_eval e ->
      let* as_e, ty_e, eff_e = Expr.gen e in
      return (as_e, Pattern.BoundVars.empty, Some ty_e, eff_e)
  | Str_exception (Ident name as id) ->
      let* var_exn = fresh_var in
      let* () =
        add_constrs [!var_exn == Ty.exn (Ty_con (Ident ("_" ^ name), []))]
      in

      let* eff = fresh_eff in
      return (As.empty, Pattern.BoundVars.singleton id var_exn, None, eff)
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

      return (as_pat ++ as_e, bounds_pat, None, eff_e)
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

      return (as_e -- [id], Pattern.BoundVars.singleton id var_id, None, eff_e)
