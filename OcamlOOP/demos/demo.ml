(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib.Interpreter

let () =
  let input = Stdio.In_channel.(input_all stdin) in
  eval_with_printing input
;;
