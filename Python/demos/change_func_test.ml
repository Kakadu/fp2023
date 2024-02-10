open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res = parser {|
def w():
    return 1
def w():
    return 2
print(w())|} in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
