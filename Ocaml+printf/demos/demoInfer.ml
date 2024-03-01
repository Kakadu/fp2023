(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Ok (env, _) -> Format.printf "%a" Inferencer.TypeEnv.pp_env env
     | Error err -> Format.printf "%a" Inferencer.pp_error err)
;;
