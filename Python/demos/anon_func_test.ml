open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let anon =
  let res = parser {|
Successor = lambda x : (x + 1)
print(Successor(0))|} in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
