open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_7 =
  let res =
    parser
      {|
def funcToAdd():
    return "Expected output"
class myClass:
    def met1():
        return 1
setattr(myClass, funco, funcToAdd)        
print(myClass.funco())|}
  in
  match res with
  | Ok v -> interpret v
  | Error v -> Error v
;;
