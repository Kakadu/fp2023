open! Base
open Ast

let gen = function
  | Str_eval e ->
      Expr.gen e
  | Str_value (Nonrecursive, [{pat= Pat_var _; expr= e}]) ->
      Expr.gen e
  | Str_value (_, _) ->
      failwith "not implemented"
