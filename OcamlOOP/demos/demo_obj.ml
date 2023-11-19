(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib.Parser
open OcamlOOP_lib.Ast
open Angstrom

(* instance from https://v2.ocaml.org/manual/objectexamples.html*)
let input =
"
let minmax x y =
  if x < y
  then
    object
      method min = x
      method max = y
    end
  else
    object
      method min = y
      method max = x
    end
"
;;

let test_parse =
  let res = parse_string ~consume:Prefix pexpr input in
  match res with
  | Ok v -> print_endline (show_exp v)
  | Error v -> print_endline v
;;
