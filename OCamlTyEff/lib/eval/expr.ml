(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Values

open EvalMonad.Let
open EvalMonad.Let_syntax
open EvalMonad.Eval

let eval_const = function
  | (Const_integer _ | Const_char _ | Const_string _) as const ->
      return @@ Val.Val_const const

let rec eval_cases val_match ~final_error cases =
  let rec helper = function
    | {left= pat; right= expr} :: case_rest ->
        let m =
          let* bounds = Pattern.eval pat val_match in
          return (bounds, expr)
        in
        catch m (function
          | Exception (Val_con (Ident "Match_failure", None)) ->
              helper case_rest
          | err ->
              fail err )
    | [] ->
        fail final_error
  in
  let* bounds, expr = helper cases in
  extend_bounds bounds (eval expr)

and eval_apply f arg =
  match f with
  | Val.Val_fun (Fun_closure (pats, expr, defenv)) -> (
    match pats with
    | pat1 :: [] ->
        let* bounds = Pattern.eval pat1 arg in
        with_bounds (Bounds.merge (force defenv) bounds) (eval expr)
    | pat1 :: pat_rest ->
        let* bounds = Pattern.eval pat1 arg in
        let defenv = lazy (Bounds.merge (force defenv) bounds) in
        return @@ Val.Val_fun (Fun_closure (pat_rest, expr, defenv))
    | [] ->
        fail TypeError )
  | Val_fun (Fun_cases (cases, defenv)) ->
      with_bounds (force defenv)
        (eval_cases arg cases ~final_error:Pattern.match_failure)
  | Val_fun (Fun_builtin funct) ->
      funct arg
  | _ ->
      fail TypeError

and eval_let_rec bindings =
  let* id, expr1 =
    match bindings with
    | [{pat; expr}] -> (
      match pat with Pat_var id -> return (id, expr) | _ -> fail TypeError )
    | _ ->
        fail @@ NotImplemented "mutually recursive bindings"
  in
  match expr1 with
  | Exp_fun (pats, expr) ->
      let* env = get_env in
      let rec defenv = lazy (Bounds.merge (Env.get_bounds env) (force self_env))
      and self_env =
        lazy (Bounds.singleton id (Val_fun (Fun_closure (pats, expr, defenv))))
      in
      return @@ force self_env
  | Exp_function cases ->
      let* env = get_env in
      let rec defenv = lazy (Bounds.merge (Env.get_bounds env) (force self_env))
      and self_env =
        lazy (Bounds.singleton id (Val_fun (Fun_cases (cases, defenv))))
      in
      return @@ force self_env
  | _ ->
      let* value1 = eval expr1 in
      return @@ Bounds.singleton id value1

and eval = function
  | Exp_ident id ->
      let* env = get_env in
      Bounds.find (Env.get_bounds env) id
      |> Option.value_map ~f:return ~default:(fail TypeError)
  | Exp_constant const ->
      eval_const const
  | Exp_let (Nonrecursive, bindings, expr2) ->
      let* bounds =
        EvalMonad.List.fold bindings ~init:Bounds.empty
          ~f:(fun acc {pat= pat1; expr= expr1} ->
            let* value1 = eval expr1 in
            let* bounds = Pattern.eval pat1 value1 in
            return @@ Bounds.merge acc bounds )
      in
      extend_bounds bounds (eval expr2)
  | Exp_let (Recursive, bindings, expr2) ->
      let* bounds = eval_let_rec bindings in
      extend_bounds bounds (eval expr2)
  | Exp_fun (pats, expr) ->
      let* env = get_env in
      let defenv = lazy (Env.get_bounds env) in
      return @@ Val.Val_fun (Fun_closure (pats, expr, defenv))
  | Exp_function cases ->
      let* env = get_env in
      let defenv = lazy (Env.get_bounds env) in
      return @@ Val.Val_fun (Fun_cases (cases, defenv))
  | Exp_apply (expr_fun, expr_arg) ->
      let* val_fun = eval expr_fun in
      let* val_arg = eval expr_arg in
      eval_apply val_fun val_arg
  | Exp_tuple exprs ->
      let* vals = EvalMonad.List.map exprs ~f:eval in
      return @@ Val.Val_tuple vals
  | Exp_ifthenelse (expr_cond, expr_th, expr_el) -> (
      let* val_cond = eval expr_cond in
      match val_cond with
      | Val_con (Ident "true", None) ->
          eval expr_th
      | Val_con (Ident "false", None) ->
          Option.value_map expr_el ~f:eval ~default:(return Val.unit)
      | _ ->
          fail TypeError )
  | Exp_match (expr_match, cases) ->
      let* val_match = eval expr_match in
      eval_cases val_match cases ~final_error:Pattern.match_failure
  | Exp_construct (id, arg) -> (
    match arg with
    | Some arg ->
        let* val_arg = eval arg in
        return @@ Val.Val_con (id, Some val_arg)
    | None ->
        return @@ Val.Val_con (id, None) )
  | Exp_sequence (expr1, expr2) ->
      let* _ = eval expr1 in
      eval expr2
  | Exp_try (expr_try, cases) ->
      catch (eval expr_try) (fun error ->
          let* exc =
            match error with Exception exc -> return exc | err -> fail err
          in
          eval_cases exc cases ~final_error:error )
