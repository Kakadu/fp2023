open! Base
open Ast

let id name = Ident.Ident name

let var x = Ty.Ty_var (Var x)

let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, ty_res)

let no_vars ty = Scheme.Forall (VarSet.empty, ty)

let basic_arith = Ty.int @> Ty.int @> Ty.int

let env =
  Env.of_alist_exn
    [ (id "id", Forall (VarSet.singleton @@ Var "a", var "a" @> var "a"))
    ; (id "+", no_vars basic_arith)
    ; (id "-", no_vars basic_arith)
    ; (id "*", no_vars basic_arith)
    ; (id "=", no_vars (Ty.int @> Ty.int @> Ty.bool))
    ; (id "true", no_vars Ty.bool)
    ; (id "false", no_vars Ty.bool)
    ; (id "()", no_vars Ty.unit)
    ; ( id "Some"
      , Forall
          ( VarSet.singleton @@ Var "a"
          , var "a" @> Ty_con (Ident "option", [var "a"]) ) )
    ; ( id "None"
      , Forall (VarSet.singleton @@ Var "a", Ty_con (Ident "option", [var "a"]))
      ) ]
