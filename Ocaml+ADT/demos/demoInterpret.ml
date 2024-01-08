(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Ok parse_result ->
    (match Interpreter.InterpreterResult.exec_program parse_result with
     | Ok actual -> Format.printf "%a" InterpreterTypes.pp_env actual
     | Error err -> Format.printf "%a" InterpreterTypes.pp_failure err)
  | Error err -> Format.printf "Parsing error: %s\n" err
;;
