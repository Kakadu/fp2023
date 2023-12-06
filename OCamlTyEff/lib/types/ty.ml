open! Base
open Ast

type t =
  | Ty_var of Var.t
  | Ty_arr of t * t
  | Ty_tuple of t list
  | Ty_con of Ident.t * t list
[@@deriving eq, ord, sexp_of, show {with_path= false}]

let unit = Ty_con (Ident "unit", [])

let int = Ty_con (Ident "int", [])

let bool = Ty_con (Ident "bool", [])

let char = Ty_con (Ident "char", [])

let string = Ty_con (Ident "string", [])

let rec vars = function
  | Ty_var x ->
      VarSet.singleton x
  | Ty_arr (ty1, ty2) ->
      VarSet.union (vars ty1) (vars ty2)
  | Ty_tuple tys ->
      List.map ~f:vars tys |> VarSet.union_list
  | Ty_con _ ->
      VarSet.empty
