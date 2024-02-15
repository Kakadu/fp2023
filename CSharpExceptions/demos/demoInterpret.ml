(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: CC0-1.0 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  Ml_tests.Interpreter_tests.interpret_wrap s
;;
