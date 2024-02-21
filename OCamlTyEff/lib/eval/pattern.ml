(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Ast
open Values

open EvalMonad.Let
open EvalMonad.Let_syntax
open EvalMonad.Eval

let match_failure = EvalError.Exception (Val_con (Ident "Match_failure", None))

let rec eval pat value =
  match pat with
  | Pat_any ->
      return Bounds.empty
  | Pat_var id ->
      return (Bounds.singleton id value)
  | Pat_constant const1 -> (
    match value with
    | Val_const const2 when Ast.equal_constant const1 const2 ->
        return Bounds.empty
    | _ ->
        fail match_failure )
  | Pat_tuple pats -> (
      let* vals =
        match value with
        | Val_tuple vals ->
            return vals
        | _ ->
            fail match_failure
      in
      let bounds =
        List.fold2 pats vals ~init:(return Bounds.empty)
          ~f:(fun acc pat value ->
            let* new_bounds = eval pat value in
            let* acc = acc in
            return @@ Bounds.merge acc new_bounds )
      in
      match bounds with
      | Ok bounds ->
          bounds
      | Unequal_lengths ->
          fail match_failure )
  | Pat_construct (pat_id, pat_arg) -> (
      let* val_arg =
        match value with
        | Val_con (val_id, val_arg) when Ident.equal pat_id val_id ->
            return val_arg
        | _ ->
            fail match_failure
      in
      match (pat_arg, val_arg) with
      | Some pat_arg, Some val_arg ->
          eval pat_arg val_arg
      | None, None ->
          return Bounds.empty
      | _ ->
          fail match_failure )
  | Pat_or _ ->
      fail @@ NotImplemented "or pattern"
