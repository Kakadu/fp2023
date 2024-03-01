(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Statement
open Angstrom

let parse_ast =
  let comment = char ';' *> skip_while (fun c -> not (is_end_of_line c)) <?> "comment" in
  skip_empty_lines
  *> sep_by1 (end_of_line *> skip_empty_lines) (trim parse_statement <* option () comment)
  <* skip_empty_lines
;;

let parse parser str = Angstrom.parse_string ~consume:Consume.Prefix parser str

let parse_show parser show str =
  match parse parser str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s\n" (show ast)
;;
