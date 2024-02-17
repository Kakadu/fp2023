(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Interpreter
open State
open StateErrorMonad
open GlobalState
open Parser

let () =
  let input = Stdio.In_channel.input_all stdin in
  let parsed_ast =
    match parse parse_ast input with
    | Result.Error e ->
      Format.printf "Error: %s" e;
      exit 1
    | Result.Ok x -> x
  in
  parse_show parse_ast show_ast input;
  let eval = eval_ast parsed_ast in
  let state, result = run eval initial_state in
  match result with
  | Result.Ok _ -> Format.printf "%s" (GlobalState.show state)
  | Result.Error err -> Format.printf "%s" err
;;
