open! Base
open Types
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
      let pat, e =
        match bindings with
        | [{pat; expr}] ->
            (pat, expr)
        | _ ->
            failwith "not implemented"
      in

      let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
      let* as_e, ty_e = Expr.gen e in

      let* () = add_constrs [ty_pat == ty_e] in

      return (as_pat ++ as_e, bounds_pat, ty_e)
  | Str_value (_, _) ->
      failwith "not implemented"
