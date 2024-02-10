open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let class_test =
  let res =
    parser
      {|
class MyClass:
    def method1():
        return 3
print(1 + MyClass.method1())|}
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
