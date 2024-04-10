(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib.Print

let () =
  let input = Stdio.In_channel.input_all Caml.stdin in
  print_stdout input
;;
