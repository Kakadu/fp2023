(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_reserved str =
  let get_lowercase_names desc =
    List.map (fun (fst, _) -> String.lowercase_ascii fst) desc
  in
  let names =
    List.concat
      [ get_lowercase_names Variants_of_mnemonic.descriptions
      ; get_lowercase_names Variants_of_register.descriptions
      ]
  in
  List.mem (String.lowercase_ascii str) names
;;

let is_end_of_line c = Char.equal c '\n'

let is_decimal_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_label_char = function
  | '_' | '$' | '#' | '@' | '~' | '.' | '?' -> true
  | c -> is_decimal_digit c || is_letter c
;;

let space = satisfy is_space *> return () <?> "space"
let spaces = skip_many space <?> "spaces"
let trim p = spaces *> p <* spaces
let comma = trim (char ',') <?> "comma"
let comment = char ';' *> skip_while (fun c -> not (is_end_of_line c)) <?> "comment"
let between t1 t2 e1 = t1 *> trim e1 <* t2

let skip_empty_lines =
  skip_many (space <|> end_of_line <|> comment) <?> "skip_empty_lines"
;;

let label_name =
  let is_valid_first_letter c = is_letter c || Char.equal '_' c in
  let* s =
    lift2
      (fun c s -> Char.escaped c ^ s)
      (satisfy is_valid_first_letter)
      (take_while is_label_char)
  in
  if is_reserved s then fail "This lable name is reserved" else return s
;;
