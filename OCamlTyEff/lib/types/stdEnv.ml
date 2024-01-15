(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Vars
open Misc

let id name = Ident.Ident name

let var x = Ty.Ty_var (Var x)
let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, Eff_total, ty_res)
let tuple tys = Ty.Ty_tuple tys

let alpha = var "a"
let no_vars ty = Scheme.Forall (VarSet.empty, ty)
let single_var ty = Scheme.Forall (VarSet.singleton_ty @@ Var "a", ty)
let single_eff lbl = Eff.Eff_row (lbl, Eff_total)

(* ======= Arithmetic operators ======= *)
let arith = Ty.int @> Ty.int @> Ty.int
let env_arith =
  [ (id "+", no_vars arith)
  ; (id "-", no_vars arith)
  ; (id "*", no_vars arith)
  ; (id "=", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id "<", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id ">", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id ">=", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id "<=", no_vars (Ty.int @> Ty.int @> Ty.bool)) ]

(* ======= Built-in constructors ======= *)
let arith = Ty.int @> Ty.int @> Ty.int
let alpha_option = Ty.Ty_con (id "option", [alpha])
let alpha_list = Ty.Ty_con (id "list", [alpha])
let env_constructors =
  [ (id "true", no_vars Ty.bool)
  ; (id "false", no_vars Ty.bool)
  ; (id "()", no_vars Ty.unit)
  ; (id "Some", single_var (alpha @> alpha_option))
  ; (id "None", single_var alpha_option)
  ; (id "[]", single_var alpha_list)
  ; (id "::", single_var (tuple [alpha; alpha_list] @> alpha_list)) ]

(* ======= Built-in exceptions ======= *)
let exn_ty name = Ty.exn (Ty.Ty_con (id @@ "_" ^ name, []))
let env_exceptions =
  [(id "Exc1", no_vars @@ exn_ty "Exc1"); (id "Exc2", no_vars @@ exn_ty "Exc2")]

(* ======= Built-in functions ======= *)
let env_functions =
  [ (id "id", single_var (alpha @> alpha))
  ; ( id "print_string"
    , no_vars (Ty_arr (Ty.string, single_eff @@ Eff.Label.console (), Ty.unit))
    )
  ; ( id "raise"
    , Scheme.Forall
        ( VarSet.of_list [Var_ty (Var "a"); Var_ty (Var "b")]
        , Ty_arr (Ty.exn alpha, single_eff @@ Eff.Label.exn @@ var "a", var "b")
        ) )
  ; ( id "ref"
    , single_var (Ty_arr (alpha, single_eff @@ Eff.Label.ref (), Ty.ref alpha))
    )
  ; ( id "!"
    , single_var (Ty_arr (Ty.ref alpha, single_eff @@ Eff.Label.ref (), alpha))
    )
  ; ( id ":="
    , single_var
        (Ty.ref alpha @> Ty_arr (alpha, single_eff @@ Eff.Label.ref (), Ty.unit))
    ) ]

let env =
  List.concat [env_arith; env_constructors; env_exceptions; env_functions]
  |> Env.of_alist_exn ~weak_counter:1
