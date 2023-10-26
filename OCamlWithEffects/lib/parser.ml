(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

type dispatch =
  { parse_bin_op : dispatch -> expr Angstrom.t
  ; parse_un_op : dispatch -> expr Angstrom.t
  ; parse_application : dispatch -> expr Angstrom.t
  ; parse_fun : dispatch -> expr Angstrom.t
  ; parse_if_then_else : dispatch -> expr Angstrom.t
  ; parse_expression : dispatch -> expr Angstrom.t
  }

(* Constructors for expressions *)
let econst x = EConst x
let ebinop op left_op right_op = EBinaryOperation (op, left_op, right_op)
let eunop operator operand = EUnaryOperation (operator, operand)
let identifier x = EIdentifier x
let eapplication f x = EApplication (f, x)
let efun var_list expression = EFun (var_list, expression)

let declraration func_name var_list expression =
  EDeclaration (func_name, var_list, expression)
;;

let rec_declraration func_name var_list expression =
  EDeclaration (func_name, var_list, expression)
;;

let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)
(* ---------------- *)

let is_keyword = function
  | "let" | "rec" | "match" | "with" | "if" | "then" | "else" | "in" | "fun" | "and" ->
    true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_wspace = skip_while is_whitespace

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_ident c = is_lower c || is_upper c || c = '_'

let is_acceptable_fl = function
  | Some c when is_lower c || c = '_' -> return c
  | _ -> fail "abc"
;;

let ident =
  skip_wspace *> peek_char
  >>= is_acceptable_fl
  >>= fun _ ->
  take_while is_ident
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: name is used as keyword"
  else return @@ EIdentifier s
;;

let is_letter c = is_upper c || is_lower c
let parens p = skip_wspace *> char '(' *> p <* skip_wspace <* char ')'

let parse_name =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|> take_while1 (fun x ->
        is_lower x || is_upper x || is_digit x || x = '\'' || x = '_'))
;;

let parse_uncapitalized_name =
  parse_name
  >>= fun name ->
  if is_lower name.[0] || name.[0] = '_'
  then return name
  else fail "Parsing error: not an uncapitalized entity."
;;

let parse_const =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let parse_int = take_while1 is_digit >>| int_of_string >>| fun x -> Int x
      and parse_str =
        char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> String x
      and parse_char = char '\'' *> any_char <* char '\'' >>| fun x -> Char x
      and parse_bool =
        string "true" <|> string "false" >>| bool_of_string >>| fun x -> Bool x
      and parse_unit = string "()" >>| fun _ -> Unit in
      let parse_const =
        choice [ parse_int; parse_str; parse_char; parse_bool; parse_unit ]
      in
      lift econst parse_const)
;;

let parse_fun pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_application pack
      ; self
      ; pack.parse_if_then_else pack
      ; parse_const
      ; ident
      ]
  in
  parens self
  <|> string "fun"
      *> lift2
           efun
           (many1 parse_uncapitalized_name <* skip_wspace <* string "->" <* skip_wspace)
           (parse_expr <* skip_wspace)
;;

let parse_if_then_else pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_application pack
      ; self
      ; pack.parse_fun pack
      ; parse_const
      ; ident
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif_then_else
           parse_expr
           (skip_wspace *> string "then" *> parse_expr)
           (skip_wspace *> string "then" *> parse_expr)
;;
