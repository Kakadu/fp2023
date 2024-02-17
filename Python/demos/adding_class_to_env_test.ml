open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let res =
  let rec print_classes : environment list -> unit = function
    | [] -> ()
    | classs :: remaining_classes ->
      print_string (get_str_from_identifier classs.id);
      print_string " ";
      print_classes remaining_classes
  in
  let stmts =
    parser
      "class MyClass:\n\tdef method1(x):\n\t\treturn 3\n\tdef method2():\n\t\treturn 2"
  in
  match stmts with
  | Ok ast ->
    let env = interpret ast in
    (match env with
     | Ok env -> print_classes env.classes
     | Error _ -> print_string "change print func test failed")
  | Error _ -> print_string "change print func test failed"
;;
