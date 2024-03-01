(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom

let pp printer parser str =
  Stdlib.Format.printf "%a" printer
  @@ Result.ok_or_failwith
  @@ Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
;;

let skip_whitespaces = skip_while Char.is_whitespace
let parse_line_comment = string "--" *> many_till any_char (string "\n") *> return ()
let parse_block_comment = string "--[[" *> many_till any_char (string "--]]") *> return ()
let parse_comment = skip_whitespaces *> (parse_block_comment <|> parse_line_comment)
let ws = many parse_comment *> skip_whitespaces

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** ======= Ident parser ======= *)

let is_keyword = function
  | "and"
  | "break"
  | "do"
  | "else"
  | "elseif"
  | "end"
  | "false"
  | "for"
  | "function"
  | "goto"
  | "if"
  | "in"
  | "local"
  | "nil"
  | "not"
  | "or"
  | "repeat"
  | "return"
  | "then"
  | "true"
  | "until"
  | "while" -> true
  | _ -> false
;;

let parse_ident =
  let parse_first =
    satisfy (function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false)
    >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false)
  in
  let* ident = lift2 String.( ^ ) parse_first parse_rest in
  if is_keyword ident then fail "ident can't be a keyword" else return ident
;;
