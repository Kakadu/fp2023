open Python_Lib.Parser
open Python_Lib.Ast

let test_parse =
  let res =
    parser
      {|
def factorial(x):
    if (x == 1):
        return 1
    else:
        return (x * factorial(x - 1))
|}
  in
  match res with
  | Ok v -> List.iter (fun e -> print_endline (show_statement e)) v
  | Error v -> print_endline v
;;
