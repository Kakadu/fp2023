open! Base

type t =
  | Ty_var of Var.t
  | Ty_arr of t * t
  | Ty_tuple of t list
  | Ty_con of Ast.Ident.t * t list
[@@deriving show {with_path= false}]

