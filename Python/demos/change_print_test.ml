open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let unpacker (Ok x) = x

let env =
  unpacker (interpret (unpacker (parser "\ndef print():\n    return 1\ntest = print()")))
;;

let res = print_vars env.vars
