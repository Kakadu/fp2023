open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let unpacker (Ok x) = x

let env =
  unpacker (interpret (unpacker (parser "\ndef print():\n    return 1\ntest = print()")))
;;

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
       | Error e -> print_string "Test change failed")
  in
  print_vars env.vars
;;
