open Miniml_lib

let () =
  let input = {| let rec fib x = if x = 1 then x else x * fib (x - 1) |} in
  match Parser.parse input with
  | Result.Ok res -> Format.printf "%a\n" Ast.pp_expr res
  | _ -> Format.printf "Error"
;;
