open! Base

type t =
  | Ty_var of Var.t
  | Ty_arr of t * t
  | Ty_tuple of t list
  | Ty_con of Ast.Ident.t * t list
[@@deriving ord, sexp_of, show {with_path= false}]

let rec vars = function
  | Ty_var x ->
      VarSet.singleton x
  | Ty_arr (ty1, ty2) ->
      VarSet.union (vars ty1) (vars ty2)
  | Ty_tuple tys ->
      List.map ~f:vars tys |> VarSet.union_list
  | Ty_con _ ->
      VarSet.empty
