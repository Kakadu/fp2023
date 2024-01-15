(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Misc

module Prelude = Prelude

let id name = Ident.Ident name

let sc str =
  let ty = Parse.parse_ty_exn str in
  Scheme.Forall (Ty.vars ty, ty)

(* ======= Arithmetic operators ======= *)
let sc_arith = sc "int -> int -> int"
let sc_compar = sc "int -> int -> bool"
let env_arith =
  [ (id "+", sc_arith)
  ; (id "-", sc_arith)
  ; (id "*", sc_arith)
  ; (id "=", sc_compar)
  ; (id "<", sc_compar)
  ; (id ">", sc_compar)
  ; (id ">=", sc_compar)
  ; (id "<=", sc_compar) ]

(* ======= Built-in constructors ======= *)
let env_constructors =
  [ (id "true", sc "bool")
  ; (id "false", sc "bool")
  ; (id "()", sc "unit")
  ; (id "Some", sc "'a -> 'a option")
  ; (id "None", sc "'a option")
  ; (id "[]", sc "'a list")
  ; (id "::", sc "('a * 'a list) -> 'a list") ]

(* ======= Built-in functions ======= *)
let var x = Ty.Ty_var (Var x)
let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, Eff_total, ty_res)

let alpha = var "a"
let single_var ty = Scheme.Forall (VarSet.singleton_ty @@ Var "a", ty)
let eff lbl = Eff.Eff_row (lbl, Eff_total)
let eff_ref = eff @@ Eff.Label.ref ()

let env_functions : (Ident.t * Scheme.t) list =
  [ ( id "print_string"
    , Forall
        (VarSet.empty, Ty_arr (Ty.string, eff @@ Eff.Label.console (), Ty.unit))
    )
  ; ( id "raise"
    , Forall
        ( VarSet.of_list [Var_ty (Var "a"); Var_ty (Var "b")]
        , Ty_arr (Ty.exn alpha, eff @@ Eff.Label.exn @@ var "a", var "b") ) )
  ; (id "ref", single_var (Ty_arr (alpha, eff_ref, Ty.ref alpha)))
  ; (id "!", single_var (Ty_arr (Ty.ref alpha, eff_ref, alpha)))
  ; (id ":=", single_var (Ty.ref alpha @> Ty_arr (alpha, eff_ref, Ty.unit))) ]

let ty_env =
  List.concat [env_arith; env_constructors; env_functions]
  |> Env.of_alist_exn ~weak_counter:1
