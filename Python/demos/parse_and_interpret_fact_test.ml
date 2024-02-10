open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res =
    parser
      {|
def factorial(x):
    if (x == 1):
        return 1
    else:
        return (x * factorial(x - 1))
print(factorial(7))|}
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
