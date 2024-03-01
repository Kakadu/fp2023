(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Lib.Interpreter.Eval (Lib.Utils.Result)

let run base inp =
  match Lib.Parser.parse inp with
  | Result.Ok x ->
    (match eval base x with
     | Ok table -> Format.printf "%s\n" (Lib.Types.Table.show_table table)
     | Error e -> Format.printf "Execute error: %s\n" (Lib.Utils.error_to_string e))
  | Error e -> Format.printf "Parsing error: %s\n" e
;;

(** Example:
    time_run
    "/home/.../Desktop/fp2023/mini-SQL/test_data/data"
    "SELECT table0.id FROM table0 WHERE table0.id > 10";; *)

let time_run base inp =
  let start = Sys.time () in
  let res = run base inp in
  Format.printf "\nExecution time: %f\n" (Sys.time () -. start);
  res
;;
