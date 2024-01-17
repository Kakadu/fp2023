(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Lib.Interpreter.Eval (Lib.Utils.Result)

let base = "path_to_testdata";;

let () =
  let inp = Stdio.In_channel.input_all Caml.stdin in
  Format.printf "%s\n" (Sys.getcwd ()); match Lib.Parser.parse inp with
  | Result.Ok x ->
    (match eval base x with
     | Ok table -> Format.printf "%s\n" (Lib.Types.Table.show_table table)
     | Error e -> Format.printf "Execute error: %s\n" (Lib.Utils.error_to_string e))
  | Error e -> Format.printf "Parsing error: %s\n" e
;;
