(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib

let parse s =
  match Parser.parse s with
  | Ok a -> a
  | Error err ->
    Format.printf "%s\n" err;
    Stdlib.exit 1
;;

let () =
  In_channel.(input_all stdin)
  |> parse
  |> Ast.show_prog
  |> Out_channel.(output_string stdout)
;;
