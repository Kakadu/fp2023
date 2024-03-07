(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Cypher_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let rs = String.split_on_char ';' s in
  let g = Interpret.create_empty_graph in
  let rec helper g = function
    | [] -> ()
    | r :: rs ->
      (match Interpret.parse_and_interpret_request g r with
       | Ok (g, out) ->
         Stdlib.Format.printf "%a\n" Interpret.pp_output out;
         helper g rs
       | Error err -> Stdlib.Format.printf "%a\n" Interpret.pp_req_error err)
  in
  helper g rs
;;
