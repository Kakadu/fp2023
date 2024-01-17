(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Common
open Ast

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let hex_of_string s = int_of_string ("0x" ^ s)

let parse_hex_with_prefix =
  string "0x" *> take_while1 is_hex_digit >>| hex_of_string <?> "parse_hex_with_prefix"
;;

let parse_hex_with_postfix = take_while1 is_hex_digit <* char 'h' >>| hex_of_string
let parse_hex = choice [ parse_hex_with_prefix; parse_hex_with_postfix ] <?> "parse_hex"

let parse_decimal =
  let parse_sign = string "+" <|> string "-" <?> "sign" in
  lift2 ( ^ ) (option "" parse_sign) (take_while1 is_decimal_digit)
  >>| int_of_string
  <?> "parse_decimal"
;;

let parse_immidiate = imm_int <$> (parse_hex <|> parse_decimal) <?> "parse_imm_int"
