(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib.Parser
open OcamlOOP_lib.Ast
open Angstrom

let input = 
"
let p = 
  object (s)
    val x = 5
    method private get_x = x
    method print = print_int s#get_x
  end
"

let test_parse =
  let res = parse_string ~consume:Prefix pexpr input in
  match res with
  | Ok v -> print_endline (show_exp v)
  | Error v -> print_endline v
;;