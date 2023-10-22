(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib.Parser
open OcamlOOP_lib.Ast
open Angstrom
let test_parse = 
  let res = parse_string ~consume: Prefix pexpr "let rec fact x =  if x = 0 then 1 else x * fact (x - 1)" in 
  match res with
  | Ok v -> print_endline (show_exp v)
  | Error v -> print_endline v

