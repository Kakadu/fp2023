(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  Main.interpretate_programm s
;;
