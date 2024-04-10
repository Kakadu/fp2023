(** Copyright 2023-2024, PavlushaSource *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Result.Ok ast ->
      Format.printf "%a\n" Ast.pp_program ast
  | Error err ->
      Format.printf "Error %s" err
