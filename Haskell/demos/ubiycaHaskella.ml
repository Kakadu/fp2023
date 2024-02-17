(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib
open Interpreter

(* todo: change this abomination *)

let pp_combined_output env environment =
  let open Typedtree in
  Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
    match Env.find environment v with
    | Some value -> Format.printf "%s :: %a => %a\n" v pp_type ty Env.pp_value_t value
    | None -> Format.printf "%s :: %a => [No Value]\n" v pp_type ty)
;;

let run s =
  match Parser.parse s with
  | Ok a ->
    (match Inferencer.run_prog a with
     | Ok res ->
       (match Eval.eval_prog a with
        | Result (Ok res') -> pp_combined_output res res'
        | Result (Error err) -> Format.printf "%a" Env.pp_err err
        | _ -> ())
     | Error err -> Format.printf "%a" Typedtree.pp_error err)
  | Error err ->
    Format.printf "%s\n" err;
    exit 1
;;

let () = In_channel.(input_all stdin) |> run
