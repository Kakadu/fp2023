(** Copyright 2021-2022, Nikita Nemakin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_pv.Parser
open Ocaml_pv.Ast

let () =
  let res = parse "let rec fac n = if n < 2 then 1 else n * fac (n - 1) " in
  match res with
  | Ok ast -> Format.printf "%a\n%!" pp_structure ast
  | Error msg -> Format.printf "Error %s\n" msg
;;
