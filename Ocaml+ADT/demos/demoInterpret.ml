(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Ok parse_result ->
    (match Inferencer.run_infer_program parse_result with
     | Ok (adt_env, env) ->
       (match Interpreter.InterpreterResult.exec_program parse_result with
        | Ok res -> InterpreterResultPrinter.pp_interpret_res adt_env env res
        | Error err -> Format.printf "%a" InterpreterTypes.pp_failure err)
     | Error err -> Format.printf "%a" InferencerTypes.pp_inf_err err)
  | Error err -> Format.printf "Parsing error: %s\n" err
;;
