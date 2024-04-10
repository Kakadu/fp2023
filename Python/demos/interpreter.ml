open Python_Lib.Parser
open Python_Lib.Interpreter
open Eval (Result)

let () =
  let input = Stdio.In_channel.(input_all stdin) in
  let res = parser input in
  match res with
  | Ok v ->
    let _ = interpret v in
    Printf.printf ""
  | Error v -> Printf.printf "The error %s while interpreting" v
;;
