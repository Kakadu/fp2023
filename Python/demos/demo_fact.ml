open Angstrom
open Python_Lib.Parser
open Python_Lib.Ast 

let test_parse = 
  let res = parse_string ~consume: Prefix pyParser "\n\
     def factorial(x):\n\
    \    if (x == 1):\n\
    \        return 1\n\
    \    else:\n\
    \        return (x * factorial(x - 1))" in 
  match res with
  | Ok v -> List.iter (fun e -> print_endline (show_statement e)) v
  | Error v -> print_endline v