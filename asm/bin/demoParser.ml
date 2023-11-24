(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Stdio
open Ast

let () =
  let input = Stdio.In_channel.input_all stdin in
  Parser.parse parse_ast show_ast input
;;
