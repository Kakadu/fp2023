(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  Tests.I_tests.show_wrap s
;;
