(** Copyright 2021-2023, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DRozhkov_lib.Inter

let () =
  let str = In_channel.input_all stdin in
  run_inter str
;;
