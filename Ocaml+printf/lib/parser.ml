(** Copyright 2021-2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdlib
open Angstrom
open Ast

(* begin parser *)
let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_low_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_up_letter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter c = is_low_letter c || is_up_letter c

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* maybe not all cases are considered for now *)
let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "or"
  | "rec"
  | "then"
  | "true" -> true
  | _ -> false
;;

let take_whitespaces = take_while is_whitespace
let take_whitespaces1 = take_while1 is_whitespace
let token = take_while1 (fun c -> is_letter c || is_digit c || c = '_')

let valname =
  token
  >>= (fun s ->
        if is_keyword s
        then fail "name of value expected"
        else (
          match s.[0] with
          | c when (not (is_keyword s)) && (is_low_letter c || c = '_') -> return s
          | _ -> fail "name of value expected"))
  >>| fun s -> LCIdent s
;;

let expr_valname = take_whitespaces *> valname >>| fun x -> Expr_val x

let const_integer =
  token
  >>= fun s ->
  let cons x = Int x in
  try int_of_string s |> cons |> return with
  | Failure _ -> fail "integer expected"
;;

let expr_integer = take_whitespaces *> const_integer >>| fun x -> Expr_const x

(* fails if s with characters after that can be interpreted by Ocaml as user-defined operator *)
let op_parse_helper s =
  let second_operator_char = function
    | '$'
    | '&'
    | '*'
    | '+'
    | '-'
    | '/'
    | '='
    | '>'
    | '@'
    | '^'
    | '|'
    | '%'
    | '<'
    | '!'
    | '.'
    | ':'
    | '?'
    | '~' -> true
    | _ -> false
  in
  string s *> peek_char
  >>= function
  | Some x when second_operator_char x -> fail "unsopported operator"
  | _ -> return ""
;;

let left_bracket = take_whitespaces *> char '('
let right_bracket = take_whitespaces *> char ')'
let parenthesis p = left_bracket *> take_whitespaces *> p <* right_bracket

let mul =
  take_whitespaces *> op_parse_helper "*" *> return (fun e1 e2 -> Bin_op (Mul, e1, e2))
;;

let div =
  take_whitespaces *> op_parse_helper "/" *> return (fun e1 e2 -> Bin_op (Div, e1, e2))
;;

let add =
  take_whitespaces *> op_parse_helper "+" *> return (fun e1 e2 -> Bin_op (Add, e1, e2))
;;

let sub =
  take_whitespaces *> op_parse_helper "-" *> return (fun e1 e2 -> Bin_op (Sub, e1, e2))
;;

let rel =
  let eq =
    take_whitespaces *> op_parse_helper "=" *> return (fun e1 e2 -> Bin_op (Eq, e1, e2))
  in
  let neq =
    take_whitespaces *> op_parse_helper "<>" *> return (fun e1 e2 -> Bin_op (Neq, e1, e2))
  in
  let less =
    take_whitespaces *> op_parse_helper "<" *> return (fun e1 e2 -> Bin_op (Less, e1, e2))
  in
  let leq =
    take_whitespaces *> op_parse_helper "<=" *> return (fun e1 e2 -> Bin_op (Leq, e1, e2))
  in
  let gre =
    take_whitespaces *> op_parse_helper ">" *> return (fun e1 e2 -> Bin_op (Gre, e1, e2))
  in
  let geq =
    take_whitespaces *> op_parse_helper ">=" *> return (fun e1 e2 -> Bin_op (Geq, e1, e2))
  in
  choice [ eq; neq; less; leq; gre; geq ]
;;

let keyword s =
  take_whitespaces *> take_while (fun c -> is_letter c || is_digit c || c == '_')
  >>= fun res -> if res = s then return s else fail "keyword expected"
;;

let keyword1 s =
  take_whitespaces1 *> take_while (fun c -> is_letter c || is_digit c || c == '_')
  >>= fun res -> if res = s then return s else fail "keyword expected"
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec unary_op expr_parser =
  take_whitespaces *> peek_char
  >>= function
  | Some c when c = '+' || c = '-' ->
    op_parse_helper (Base.String.of_char c)
    *> (take_whitespaces *> parenthesis (unary_op expr_parser) <|> unary_op expr_parser)
    >>| fun e -> if c = '+' then Un_op (Un_plus, e) else Un_op (Un_minus, e)
  | _ -> expr_parser
;;

let rec if_then_else expr_parser =
  lift3
    (fun e1 e2 e3 -> ITE (e1, e2, e3))
    (keyword "if" >>= fun _ -> if_then_else expr_parser <|> expr_parser)
    (keyword "then" >>= fun _ -> if_then_else expr_parser <|> expr_parser)
    (keyword "else" >>= fun _ -> if_then_else expr_parser <|> expr_parser)
;;

let expr =
  fix (fun cur_parser ->
    let cur_parser = expr_integer <|> expr_valname <|> parenthesis cur_parser in
    let cur_parser = chainl1 cur_parser (return (fun e1 e2 -> App (e1, e2))) in
    let cur_parser = unary_op cur_parser in
    let cur_parser = chainl1 cur_parser (mul <|> div) in
    let cur_parser = chainl1 cur_parser (add <|> sub) in
    let cur_parser = chainl1 cur_parser rel in
    let cur_parser = if_then_else cur_parser <|> cur_parser in
    cur_parser)
;;

let rec decl_fun expr_parser =
  valname
  >>= fun valname ->
  choice
    [ (take_whitespaces *> char '=' *> expr_parser >>| fun expr -> Fun (valname, expr))
    ; (take_whitespaces1 *> decl_fun expr_parser
       >>| fun fun_expr -> Fun (valname, fun_expr))
    ]
;;

let decl =
  keyword "let"
  *> lift3
       (fun rec_flag name expr -> Let_decl (rec_flag, name, expr))
       (option false (keyword1 "rec" >>| fun s -> String.equal s "rec"))
       (take_whitespaces1 *> valname)
       (take_whitespaces *> op_parse_helper "=" *> expr
        <|> take_whitespaces1 *> decl_fun expr)
  <* take_whitespaces
  <* option "" (string ";;")
;;

let program_parser =
  let empty_decl = many (take_whitespaces *> string ";;") in
  many1 (empty_decl *> decl <* empty_decl) <* take_whitespaces
;;

let parse s = parse_string ~consume:All program_parser s
