(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

let run_env ppf init_env code =
  let open Stdlib.Format in
  match Parse.parse code with
  | Some program ->
      List.fold program ~init:init_env ~f:(fun env_acc str_item ->
          match Infer.infer_structure_item env_acc str_item with
          | Ok (new_env, bounds, sc) ->
              Option.iter sc ~f:(fprintf ppf "%a\n" Scheme.pp) ;

              List.iter bounds ~f:(fun bound_var ->
                  let sc = Env.find_exn new_env bound_var in
                  let (Ident name) = bound_var in
                  fprintf ppf "%s: %a\n" name Scheme.pp sc ) ;
              new_env
          | Error err ->
              fprintf ppf "%a\n" Infer.TyError.pp err ;
              env_acc )
  | None ->
      Stdlib.print_endline "syntax error" ;
      init_env

let std_env =
  let prelude =
    {| exception Exc1;; exception Exc2;; |} ^ Builtin.Prelude.prelude
  in
  run_env Format.str_formatter Builtin.ty_env prelude

let run code = run_env Format.std_formatter std_env code |> ignore
