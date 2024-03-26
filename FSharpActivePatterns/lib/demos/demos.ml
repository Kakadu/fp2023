(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib.Interpret
open FSharpActivePatterns_lib.Parser
open FSharpActivePatterns_lib.Errorinter

let run input =
  match main_parse input with
  | Ok ast ->
    (match eval_program ast with
     | Ok res -> Format.printf "%s" (show_value res)
     | Error e -> Format.printf "(Error while interpreting): %a" pp_error_inter e)
  | Error e -> Format.printf "(Error while parsing): %s" e
;;

let () = run (Stdio.In_channel.input_all Caml.stdin)
