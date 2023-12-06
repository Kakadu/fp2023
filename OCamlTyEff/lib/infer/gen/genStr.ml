open! Base
open GenExpr
open Ast

let gen_str = function
  | Str_eval e ->
      gen_expr e
  | Str_value (Nonrecursive, [{pat= Pat_var _; expr= e}]) ->
      gen_expr e
  | _ ->
      failwith "not implemented"
