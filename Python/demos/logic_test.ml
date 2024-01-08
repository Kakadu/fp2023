open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res =
    parser
      "if (1 == 1 or 1/1 == 1 and 1 != 2 and 1 < 2 and 1 <= 2 and 2 > 1 and 2 >= 1 and 1 \
       % 1 != 3):\n\
       \tprint(\"Passed\")"
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
