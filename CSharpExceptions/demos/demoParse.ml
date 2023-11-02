(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: CC0-1.0 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Csharp_Exc_Lib.Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Csharp_Exc_Lib.Ast.pp_tast ast
  | Error _ -> Format.printf "Some error with parsing"
;;
