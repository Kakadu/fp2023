open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let unpacker (Ok x) = x

let res =
  let env =
    unpacker
      (interpret
         (unpacker
            (parser
               "class MyClass:\n\
                \tdef method1(x):\n\
                \t\treturn 3\n\
                \tdef method2():\n\
                \t\treturn 2")))
  in
  let rec print_classes : environment list -> unit = function
    | [] -> ()
    | classs :: remaining_classes ->
      print_string (get_str_from_identifier classs.id);
      print_string " ";
      print_classes remaining_classes
  in
  print_classes env.classes
;;
