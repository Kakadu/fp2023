(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Cypher_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_request s with
  | Ok r -> Stdlib.Format.printf "%a\n" Ast.pp_clause r
  | Error msg -> Stdlib.Format.printf "Error%s\n" msg
;;
