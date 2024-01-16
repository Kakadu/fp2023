(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib.Print

let () =
  let path = String.trim (Stdio.In_channel.input_all Caml.stdin) in
  let ic = open_in path in
  try
    let code = Stdio.In_channel.input_all ic in
    print_stdout code
  with
  | e ->
    close_in ic;
    raise e
;;
