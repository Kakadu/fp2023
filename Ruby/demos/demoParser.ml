(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ruby_lib
open Stdio

let () =
  let input = Stdio.In_channel.input_all stdin in
  Parser.interpret_parse Parser.final_parse Ast.show_ast input
;;
