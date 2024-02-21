(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Values

open EvalMonad.Let_syntax
open EvalMonad.Let
open EvalMonad.Eval

let eval = function
  | Str_eval expr ->
      let* value = Expr.eval expr in
      return (Some value, [])
  | Str_value (Nonrecursive, bindings) ->
      let* bounds =
        EvalMonad.List.fold bindings ~init:Bounds.empty
          ~f:(fun acc {pat= pat1; expr= expr1} ->
            let* value1 = Expr.eval expr1 in
            let* bounds = Pattern.eval pat1 value1 in
            return @@ Bounds.merge acc bounds )
      in
      let* env = get_env in
      let* () =
        set_env (Env.set_bounds env (Bounds.merge (Env.get_bounds env) bounds))
      in
      return (None, Bounds.idents bounds)
  | Str_value (Recursive, bindings) ->
      let* bounds = Expr.eval_let_rec bindings in
      let* env = get_env in
      let* () =
        set_env (Env.set_bounds env (Bounds.merge (Env.get_bounds env) bounds))
      in
      return (None, Bounds.idents bounds)
  | Str_exception _ ->
      return (None, [])
