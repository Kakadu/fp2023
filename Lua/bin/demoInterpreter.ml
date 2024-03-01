open Ast
open Parser
open Interpreter

let () =
  let input = In_channel.input_all stdin in
  let parsed_ast =
    match parse input with
    | Result.Error e ->
      Format.printf "Error: %s" e;
      exit 1
    | Result.Ok x -> x
  in
  eval parsed_ast
;;
