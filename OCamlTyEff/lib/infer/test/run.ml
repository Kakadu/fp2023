(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

let run code =
  let open Stdlib.Format in
  match Parse.parse code with
  | Some [str_item] -> (
    match Infer.infer_structure_item StdEnv.env str_item with
    | Ok (env, bounds, sc) ->
        printf "%a\n" Scheme.pp sc ;

        List.iter bounds ~f:(fun bound_var ->
            let sc = Env.find_exn env bound_var in
            let (Ident name) = bound_var in
            printf "%s: %a\n" name Scheme.pp sc )
    | Error err ->
        Infer.TyError.pp Stdlib.Format.std_formatter err )
  | None ->
      Stdlib.print_endline "syntax error"
  | _ ->
      Stdlib.print_endline "invalid test"
