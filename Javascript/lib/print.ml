(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
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
