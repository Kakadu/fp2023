open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let fString =
  let res = parser {|
a = "World"
print(f"Hello {a}!")|} in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
