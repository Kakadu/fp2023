open! Base
open Types
open Constraints
open Ast

open Containers
module As = Assumptions

let ( ! ) tv = Ty.Ty_var tv

let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, ty_res)

let ( == ) t1 t2 = Constr.EqConstr (t1, t2)

let ( ++ ) = As.merge

let ( -- ) asm = List.fold ~init:asm ~f:As.remove

let type_of_constant = function
  | Const_integer _ ->
      Ty.int
  | Const_char _ ->
      Ty.char
  | Const_string _ ->
      Ty.string
