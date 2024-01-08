(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast

type t =
  | Ty_var of Var.t
  | Ty_arr of t * Eff.t * t
  | Ty_tuple of t list
  | Ty_con of Ident.t * t list
[@@deriving eq, ord, sexp_of]

let rec pp ppf =
  let open Stdlib.Format in
  let pp_raw = pp in
  let pp ppf ty =
    match ty with
    | Ty_arr _ | Ty_tuple _ ->
        (* wrap arrow and tuple types in parentheses *)
        fprintf ppf "(%a)" pp_raw ty
    | _ ->
        pp_raw ppf ty
  in
  function
  | Ty_var var ->
      Var.pp ppf var
  | Ty_arr (l, eff, r) ->
      fprintf ppf "%a %a-> %a" pp l Eff.pp eff pp_raw r
  | Ty_tuple tys ->
      let pp_tys = pp_print_list pp ~pp_sep:(fun out () -> fprintf out " * ") in
      fprintf ppf "%a" pp_tys tys
  | Ty_con (Ident name, args) -> (
      let pp_args = pp_print_list pp ~pp_sep:(fun out () -> fprintf out ",@") in
      match args with
      | [] ->
          fprintf ppf "%s" name
      | [arg] ->
          fprintf ppf "%a %s" pp arg name
      | _ ->
          fprintf ppf "(%a) %s" pp_args args name )

let unit = Ty_con (Ident "unit", [])
let int = Ty_con (Ident "int", [])
let bool = Ty_con (Ident "bool", [])
let char = Ty_con (Ident "char", [])
let string = Ty_con (Ident "string", [])

let rec vars = function
  | Ty_var x ->
      VarSet.singleton_ty x
  | Ty_arr (ty1, eff, ty2) ->
      VarSet.union_list [vars ty1; Eff.vars eff; vars ty2]
  | Ty_tuple tys ->
      List.map ~f:vars tys |> VarSet.union_list
  | Ty_con (_, tys) ->
      List.map ~f:vars tys |> VarSet.union_list
