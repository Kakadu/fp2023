(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Interpreter
open Ast

let pp_ok result = Format.printf "%a" pp_statement @@ Result.get_ok result

let pp_error result =
  match Result.get_error result with
  | `ParsingError s -> Format.eprintf "Error: %s" s
;;

let pp ?(parse = parse) str =
  let result = parse str in
  if Result.is_ok result then pp_ok result else pp_error result
;;

let print_return str =
  match interpret str with
  | Result.Ok x -> Format.eprintf "Programm return: %s" x.return
  | Result.Error x -> Format.eprintf "Error: %s" x
;;

let print_output str =
  match interpret str with
  | Result.Ok x ->
    Format.eprintf "Programm output: %s\nProgramm return: %s" x.stdout x.return
  | Result.Error x -> Format.eprintf "Error: %s" x
;;
