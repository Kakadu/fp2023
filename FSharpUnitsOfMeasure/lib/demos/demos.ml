(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib.Interpreter
open FSharpUnitsOfMeasure_lib.Parser
open FSharpUnitsOfMeasure_lib.Interpret_error

let parsing_and_inerpretation input =
  match parser input with
  | Ok ast ->
    (match interpreter ast with
     | Ok (_, res) -> pp_value Format.std_formatter res
     | Error e -> Format.printf "Interpretation error. %a" print_error e)
  | Error e -> Format.printf "Parsing error %s" e
;;

let () = parsing_and_inerpretation (Stdio.In_channel.input_all Caml.stdin)
