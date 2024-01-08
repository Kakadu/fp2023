(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast
  | Error _ -> Format.printf "Some error"
;;
