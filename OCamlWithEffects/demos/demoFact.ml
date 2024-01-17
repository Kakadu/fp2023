open Ocaml_with_effects_lib
open Parser
open Ast

let () =
  let s = "let rec fact n = if n = 1 then 1 else n * fact (n - 1)" in
  match parse s with
  | Result.Ok ast -> Format.printf "%a\n" pp_program ast
  | Error _ -> Format.printf "Error"
;;
