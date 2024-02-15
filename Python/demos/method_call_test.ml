open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let class_test =
  let res =
    parser "class MyClass:\n\tdef method1():\n\t\treturn 3\nprint(1 + MyClass.method1())"
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
