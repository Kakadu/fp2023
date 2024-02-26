(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let print_res typed_env env_values =
  let open Interpreter in
  let open Inferencer in
  let std = EnvValues.std in
  let without_std =
    Base.Map.filter_keys env_values ~f:(fun name ->
      match EnvValues.find std name with
      | None -> true
      | Some _ -> false)
  in
  Base.Map.iteri without_std ~f:(fun ~key ~data ->
    match TypeEnv.find typed_env key with
    | Some sch ->
      Format.printf "val %s : %a = %a\n" key Pprint.pp_scheme_without sch pp_value data
    | None -> Format.printf "Value %s is not in typed environment\n" key)
;;

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error. %a" Inferencer.pp_error err
     | Ok (typed_env, new_ast) ->
       (match Interpreter.run_eval_program new_ast with
        | Error err -> Format.printf "Interpretation error. %a" Interpreter.pp_error err
        | Ok env_values -> print_res typed_env env_values))
;;
