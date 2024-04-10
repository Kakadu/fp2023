(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types

open Eval.EvalMonad.Let
open Eval.EvalMonad.Let_syntax
open Eval.EvalMonad.Eval
open Eval

type builtin = Val.t -> Val.t EvalMonad.t

let id name = Ident.Ident name

let sc str =
  let ty = Parse.parse_ty_exn str in
  Scheme.Forall (Ty.vars ty, ty)

let var x = Ty.Ty_var (Var x)
let alpha = var "a"

let ( @> ) (l, eff) r = Ty.Ty_arr (l, eff, r)

let no_vars ty = Scheme.Forall (VarSet.empty, ty)
let single_var ty = Scheme.Forall (VarSet.singleton_ty @@ Var "a", ty)

let eff lbl = Eff.Eff_row (lbl, Eff_total)

let extract_int = function
  | Val.Val_const (Const_integer x) ->
      return x
  | _ ->
      fail TypeError

let extract_string = function
  | Val.Val_const (Const_string x) ->
      return x
  | _ ->
      fail TypeError

let extract_char = function
  | Val.Val_const (Const_char x) ->
      return x
  | _ ->
      fail TypeError

let make_2args_fun f (value : Val.t) =
  return @@ Val.Val_fun (Fun_builtin (f value))

let make_bin_fun_int op =
  make_2args_fun (fun x y ->
      let* x = extract_int x in
      let* y = extract_int y in
      return @@ Val.Val_const (Const_integer (op x y)) )

let make_bin_fun_bool op =
  make_2args_fun (fun x y ->
      let* x = extract_int x in
      let* y = extract_int y in
      return @@ if op x y then Val.bool_true else Val.bool_false )
