open Ocamladt_lib

let () =
  let s =
    "let rec factorial_recursive = fun (n: int) -> if n <= 1 then 1 else n * \
     factorial_recursive (n - 1)"
  in
  match Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast
  | Error _ -> Format.printf "Some error"
;;
