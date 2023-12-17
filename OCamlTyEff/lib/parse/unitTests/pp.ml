(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res ->
      printer Stdlib.Format.std_formatter res
  | Error _ ->
      Stdlib.print_endline "syntax error"
