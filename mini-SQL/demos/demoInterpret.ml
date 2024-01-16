open Lib.Interpreter.Eval (Lib.Utils.Result)

let base = "/home/dmitriy/Desktop/fp2023/mini-SQL/test_data/database1"

let () =
  let inp = Stdio.In_channel.input_all Caml.stdin in
  match Lib.Parser.parse inp with
  | Result.Ok x ->
    (match eval base x with
     | Ok table -> Format.printf "Result: %s\n" (Lib.Types.Table.show_table table)
     | Error e -> Format.printf "Execute error:%s" (Lib.Utils.show_error e))
  | Error e -> Format.printf "Parsing error:%s" e
;;
