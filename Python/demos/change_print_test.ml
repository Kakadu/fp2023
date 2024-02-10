open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let res =
  let rec print_vars = function
    | [] -> ()
    | (var : var_symb) :: (remaining_vars : var_symb list) ->
      (match pack_to_string var.value with
       | Ok v ->
         print_string (get_str_from_identifier var.identifier);
         print_string " = ";
         print_string v;
         print_string " ";
         print_vars remaining_vars
       | Error _ -> print_string "Test change failed")
  in
  let stmts = parser {|
def print():
    return 1
test = print()|} in
  match stmts with
  | Ok ast ->
    let env = interpret ast in
    (match env with
     | Ok env -> print_vars env.vars
     | Error _ -> print_string "change print func test failed")
  | Error _ -> print_string "change print func test failed"
;;
