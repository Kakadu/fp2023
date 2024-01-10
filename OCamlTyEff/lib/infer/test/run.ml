(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

let run code =
  let open Stdlib.Format in
  match Parse.parse code with
  | Some program ->
      List.fold program ~init:StdEnv.env ~f:(fun env_acc str_item ->
          match Infer.infer_structure_item env_acc str_item with
          | Ok (new_env, bounds, sc) ->
              Option.iter sc ~f:(printf "%a\n" Scheme.pp) ;

              List.iter bounds ~f:(fun bound_var ->
                  let sc = Env.find_exn new_env bound_var in
                  let (Ident name) = bound_var in
                  printf "%s: %a\n" name Scheme.pp sc ) ;
              new_env
          | Error err ->
              Infer.TyError.pp Stdlib.Format.std_formatter err ;
              env_acc )
      |> ignore
  | None ->
      Stdlib.print_endline "syntax error"
