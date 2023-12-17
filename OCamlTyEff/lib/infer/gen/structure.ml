(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast

open Common
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

let gen = function
  | Str_eval e ->
      let* as_e, ty_e = Expr.gen e in
      return (as_e, Pattern.BoundVars.empty, ty_e)
  | Str_value (Nonrecursive, bindings) ->
      let* pat, e =
        match bindings with
        | [{pat; expr}] ->
            return (pat, expr)
        | _ ->
            fail @@ NotImplemented "several value bindings (and)"
      in

      let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
      let* as_e, ty_e = Expr.gen e in

      let* () = add_constrs [ty_pat == ty_e] in

      return (as_pat ++ as_e, bounds_pat, ty_e)
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

      (* XXX: check rhs of let rec.
         e.g. `let rec x = x + 1` must be rejected *)
      let* as_e, ty_e = Expr.gen e in

      let* () =
        add_constrs
          (As.lookup as_e id |> List.map ~f:(fun var_expr -> !var_expr == ty_e))
      in

      let* var_id = fresh_var in
      let* () = add_constrs [!var_id == ty_e] in

      return (as_e -- [id], Pattern.BoundVars.singleton id var_id, ty_e)
