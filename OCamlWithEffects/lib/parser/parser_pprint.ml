(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser_errors

let pp_error ppf = function
  | Syntax_error -> Format.fprintf ppf "Syntax error."
;;

let print_parser_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

let print_parser_result ast = Format.printf "%a\n" Ast.pp_program ast
