(** Copyright 2021-2022, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv.Parser

let () =
  let str = In_channel.input_all stdin in
  test_parse str
;;
