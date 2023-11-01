(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PrologDatalog_lib.Ast
open PrologDatalog_lib.Parser
open Angstrom

let () =
  let test =
    "factorial(0, 1).\n\
     factorial(N, Fact) :-\n\
     N > 0,\n\
     N1 is N - 1,\n\
     factorial(N1, Fact1),\n\
     Fact is N * Fact1."
  in
  match parse_string ~consume:Consume.All parse_prolog test with
  | Result.Ok res -> Format.printf "%a\n" pp_many_term res
  | _ -> Format.printf "Something went wrong"
;;
