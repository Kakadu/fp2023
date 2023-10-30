(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match (Llvm_lib.Parser.parse_program s) with
  | Result.Ok lst -> Format.printf "%s" lst
  | Error e -> Format.printf "Error: %s" e
;;
