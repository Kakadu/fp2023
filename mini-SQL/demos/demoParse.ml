(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

let parse inp = Angstrom.parse_string ~consume:All Lib.Parser.parse inp

let () =
  let inp = Stdio.In_channel.input_all Caml.stdin in
  match parse inp with
  | Result.Ok x -> Format.printf "Parse result: %s" (Lib.Ast.show_request x)
  | Error e -> Format.printf "Error%s" e
;;
