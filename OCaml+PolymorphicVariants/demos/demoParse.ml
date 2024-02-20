(** Copyright 2021-2022, Nikita Nemakin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_pv.Parser

let () =
  let str = In_channel.input_all stdin in
  test_parse str
;;
