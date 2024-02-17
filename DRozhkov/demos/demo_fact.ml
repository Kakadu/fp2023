(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)
open DRozhkov_lib.Ast

open DRozhkov_lib.Parser

let () =
  let test = "let rec fact x = if x = 0 then 1 else x * fact (x - 1)" in
  match parser test with
  | Result.Ok res -> Format.printf "%a\n" pp_expression res
  | _ -> Format.printf "WTF????????"
;;
