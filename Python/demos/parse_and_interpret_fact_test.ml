open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res =
    parser
      "\n\
       def factorial(x):\n\
      \    if (x == 1):\n\
      \        return 1\n\
      \    else:\n\
      \        return (x * factorial(x - 1))\n\
       print(factorial(7))"
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
