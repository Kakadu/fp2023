(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib.Parser
open OcamlOOP_lib.Ast

let test_parse = 
  let res = parse Stdio.In_channel.(input_all stdin) in 
  match res with
  | Ok v -> List.iter (fun e -> print_endline (show_exp e)) v
  | Error v -> prerr_endline v
