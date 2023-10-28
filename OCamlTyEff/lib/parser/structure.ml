(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Expr
open Pattern

(**
  [let P1 = E1 and P2 = E2 and ... and Pn = En]
  [let rec P1 PArg1 = E1 and P2 = E2 and ... and Pn = En]
*)
let parse_str_let =
  parse_let_binding parse_expression parse_pattern
  >>| fun (rec_flag, bindings) -> Str_value (rec_flag, bindings)

let parse_structure : structure t =
  let parse_structure_item =
    parse_str_let <|> (parse_expression >>| fun e -> Str_eval e)
  in
  sep_by ws parse_structure_item <* ws
