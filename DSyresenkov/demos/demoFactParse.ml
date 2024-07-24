open Miniml_lib

let () =
  let input = {| let rec fib x = if x = 1 then x else x * fib (x - 1) |} in
  match Parser.parse input with
  | Result.Ok res -> Format.printf "%a\n" Ast.pp_program res
  | _ -> Format.printf "Error"
;;

let _ = Ast.pp_is_rec
let _ = Ast.pp_binop
let _ = Ast.pp_id
let _ = Ast.pp_pattern
let _ = Ast.pp_const
let _ = Ast.show_id
let _ = Ast.show_const
let _ = Ast.show_is_rec
let _ = Ast.show_pattern
let _ = Ast.show_binop
let _ = Ast.show_expr
let _ = Ast.show_program
let _ = Interpret.pp_value
let _ = Infer.TypeEnv.empty
