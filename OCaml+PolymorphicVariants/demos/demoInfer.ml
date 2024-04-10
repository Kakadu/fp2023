(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv.Infer

let () =
  let str = In_channel.input_all stdin in
  test_infer str
;;
