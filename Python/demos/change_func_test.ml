open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res = parser "def w():\n    return 1\ndef w():\n    return 2\nprint(w())" in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
