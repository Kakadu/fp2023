(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Stdio
open Eval

let run_env ppf init_env code =
  let open Format in
  match Parse.parse code with
  | Some program ->
      List.fold program ~init:init_env ~f:(fun env str_item ->
          match eval_structure_item ~printer:print_string env str_item with
          | Ok (new_env, idents, value) ->
              Option.iter value ~f:(fprintf ppf "%a\n" (Val.pp new_env)) ;

              let bounds = Env.get_bounds new_env in
              List.iter idents ~f:(fun bound_var ->
                  let (Ident name) = bound_var in
                  let value = Bounds.find_exn bounds bound_var in
                  fprintf ppf "%s: %a\n" name (Val.pp new_env) value ) ;
              new_env
          | Error err ->
              EvalError.pp env Format.std_formatter err ;
              env )
  | None ->
      print_endline "syntax error" ;
      init_env

let std_env = run_env Format.str_formatter Builtin.eval_env Builtin.prelude

let run code =
  let _ : Env.t = run_env Format.std_formatter std_env code in
  ()
