(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

let () =
  let inp = Stdio.In_channel.input_all Caml.stdin in
  match Lib.Parser.parse inp with
  | Result.Ok x -> Format.printf "Parse result: %s" (Lib.Ast.show_request x)
  | Error e -> Format.printf "Error%s" e
;;
