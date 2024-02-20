(** Coptyright 2021-2022, Nikita Nemakin *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_pv.Interpreter

let () =
  let str = In_channel.input_all stdin in
  test_interpret str
;;
