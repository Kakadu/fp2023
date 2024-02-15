(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

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

let is_keyword = function
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "match"
  | "or"
  | "rec"
  | "then"
  | "true"
  | "with" -> true
  | _ -> false
;;

let take_whitespaces = take_while is_whitespace
let take_whitespaces1 = take_while1 is_whitespace
let token1 = take_while1 (fun c -> is_letter c || is_digit c || c = '_')

let keyword s =
  take_whitespaces *> token1
  >>= fun res -> if res = s then return s else fail "keyword expected"
;;

let whitespace1_keyword s =
  take_whitespaces1 *> token1
  >>= fun res -> if res = s then return s else fail "keyword expected"
;;

let valname =
  token1
  >>= (fun s ->
        let c = s.[0] in
        if (not (is_keyword s)) && s <> "_" && (is_low_letter c || c = '_')
        then return s
        else fail "name of value expected")
  >>| fun s -> LCIdent s
;;

let expr_valname = take_whitespaces *> valname >>| fun x -> Expr_val x

let const_integer =
  token1
  >>= fun s ->
  let cons x = Int x in
  try int_of_string s |> cons |> return with
  | Failure _ -> fail "integer expected"
;;

let expr_integer = take_whitespaces *> const_integer >>| fun x -> Expr_const x

let const_bool =
  token1
  >>= function
  | "false" -> return @@ Bool false
  | "true" -> return @@ Bool true
  | _ -> fail "Bool constant expected"
;;

let expr_bool = take_whitespaces *> const_bool >>| fun x -> Expr_const x

let const_char =
  char '\''
  *> (peek_char_fail
      >>= fun c ->
      match c with
      | '\\' ->
        peek_string 2
        >>= fun s ->
        (match s with
         | {|\n|} -> advance 2 *> return '\n'
         | {|\t|} -> advance 2 *> return '\t'
         | {|\b|} -> advance 2 *> return '\b'
         | {|\r|} -> advance 2 *> return '\r'
         | {|\\|} -> advance 2 *> return '\\'
         | {|\'|} -> advance 2 *> return '\''
         | {|\"|} -> advance 2 *> return '\"'
         | {|\ |} -> advance 2 *> return '\ '
         | ({|\x|} | {|\o|} | _) when is_digit s.[1] ->
           fail "Escape sequense is not supported"
         | _ -> fail "Illegal backslash escape in character")
      | _ -> advance 1 *> return c)
  <* char '\''
  >>| fun c -> Char c
;;

let expr_char = take_whitespaces *> const_char >>| fun c -> Expr_const c

let const_string =
  let rec helper acc =
    peek_char_fail
    >>= function
    | '"' -> advance 1 *> return acc
    | '\\' ->
      take 2
      >>= (function
       | {|\n|} -> helper (acc ^ "\n")
       | {|\t|} -> helper (acc ^ "\t")
       | {|\b|} -> helper (acc ^ "\b")
       | {|\r|} -> helper (acc ^ "\r")
       | {|\\|} -> helper (acc ^ "\\")
       | {|\'|} -> helper (acc ^ "\'")
       | {|\"|} -> helper (acc ^ "\"")
       | {|\ |} -> helper (acc ^ " ")
       | _ -> fail "Illegal (or not supported) backslash escape in string")
    | _ -> take 1 >>= fun t -> helper (acc ^ t)
  in
  char '\"' *> helper "" >>| fun s -> String s
;;

let expr_string = take_whitespaces *> const_string >>| fun c -> Expr_const c

(* fails if s with characters after that can be interpreted by OCaml as user-defined operator *)
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

let and_op =
  take_whitespaces *> op_parse_helper "&&" *> return (fun e1 e2 -> Bin_op (And, e1, e2))
;;

let or_op =
  take_whitespaces *> op_parse_helper "||" *> return (fun e1 e2 -> Bin_op (Or, e1, e2))
;;

let concat_op =
  take_whitespaces *> op_parse_helper "^" *> return (fun e1 e2 -> Bin_op (Concat, e1, e2))
;;

let concat_format_op =
  take_whitespaces
  *> op_parse_helper "^^"
  *> return (fun e1 e2 -> Bin_op (Concat_format, e1, e2))
;;

let list_cons_op =
  take_whitespaces
  *> op_parse_helper "::"
  *> return (fun e1 e2 -> Expr_cons_list (e1, e2))
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let unary_op expr_item_parser =
  fix (fun cur_expr ->
    take_whitespaces
    *> choice
         [ (op_parse_helper "+" *> cur_expr >>| fun e -> Un_op (Un_plus, e))
         ; (op_parse_helper "-" *> cur_expr >>| fun e -> Un_op (Un_minus, e))
         ; expr_item_parser
         ])
;;

let if_then_else expr_item_parser =
  lift3
    (fun e1 e2 e3 -> Expr_ite (e1, e2, e3))
    (keyword "if" *> expr_item_parser)
    (keyword "then" *> expr_item_parser)
    (keyword "else" *> expr_item_parser)
;;

let parse_tuple expr =
  expr
  >>= (fun first_expr ->
        many1 (take_whitespaces *> char ',' *> expr)
        >>| fun expr_list -> Expr_tuple (first_expr, expr_list))
  <|> expr
;;

let parse_list expr =
  let parse_empty =
    take_whitespaces *> char '[' *> take_whitespaces *> char ']'
    >>| fun _ -> Expr_empty_list
  in
  parse_empty
  <|> take_whitespaces
      *> char '['
      *> fix (fun cur_parser ->
        choice
          [ (expr
             <* take_whitespaces
             <* char ']'
             >>| fun e -> Expr_cons_list (e, Expr_empty_list))
          ; (expr
             <* take_whitespaces
             <* char ';'
             >>= fun e -> cur_parser >>| fun l -> Expr_cons_list (e, l))
          ])
;;

let parse_any_pat =
  take_whitespaces *> token1
  >>= fun s -> if s = "_" then return Pat_any else fail {|Pattern "any" expected|}
;;

let parse_val_pat = take_whitespaces *> valname >>| fun s -> Pat_val s

let parse_const_pat =
  let parse_const = choice [ const_integer; const_bool; const_char; const_string ] in
  take_whitespaces *> parse_const >>| fun c -> Pat_const c
;;

let parse_pat_empty_list =
  take_whitespaces *> char '[' *> take_whitespaces *> char ']' >>| fun _ -> Pat_empty_list
;;

let parse_pat =
  take_whitespaces
  *> fix (fun cur_pat ->
    let parse_base_pat =
      choice [ parse_any_pat; parse_val_pat; parse_const_pat; parse_pat_empty_list ]
    in
    let parse_cons_pat =
      let helper =
        take_whitespaces
        *> op_parse_helper "::"
        *> return (fun p1 p2 -> Pat_cons_list (p1, p2))
      in
      chainr1 (parenthesis cur_pat <|> parse_base_pat) helper
    in
    let parse_tuple_pat =
      parenthesis cur_pat
      <|> parse_base_pat
      >>= fun first_pat ->
      many1 (take_whitespaces *> char ',' *> (parenthesis cur_pat <|> parse_base_pat))
      >>| fun pat_list -> Pat_tuple (first_pat, pat_list)
    in
    choice [ parse_tuple_pat; parse_cons_pat; parse_base_pat; parenthesis cur_pat ])
;;

let expr_match expr =
  let case =
    take_whitespaces *> char '|' *> parse_pat
    >>= fun pat -> take_whitespaces *> op_parse_helper "->" *> expr >>| fun e -> pat, e
  in
  keyword "match" *> expr
  >>= fun e -> keyword "with" *> many1 case >>| fun l -> Expr_match (e, l)
;;

(* for parsing arguments and body of function *)
let fun_helper expr sep =
  let rec helper e = function
    | [] -> e
    | h :: tl -> Expr_fun (h, helper e tl)
  in
  many1 parse_pat
  >>= fun args ->
  take_whitespaces *> op_parse_helper sep *> expr >>| fun e -> helper e args
;;

let expr_fun expr = keyword "fun" *> take_whitespaces1 *> fun_helper expr "->"

let let_in expr =
  keyword "let"
  *> lift4
       (fun rec_flag name expr1 expr2 -> Expr_let ((rec_flag, name, expr1), expr2))
       (option false (whitespace1_keyword "rec" >>| fun _ -> true))
       (take_whitespaces1 *> valname)
       (take_whitespaces *> op_parse_helper "=" *> expr
        <|> take_whitespaces1 *> fun_helper expr "="
        <* keyword "in")
       expr
;;

let get_by_id expr =
  expr
  >>= (fun str ->
        take_whitespaces *> char '.' *> take_whitespaces *> char '[' *> expr
        >>= fun id ->
        take_whitespaces *> char ']'
        >>| fun _ -> Expr_app (Expr_app (Expr_val (LCIdent "get"), str), id))
  <|> expr
;;

let seq_op = take_whitespaces *> char ';' *> return (fun e1 e2 -> Expr_seq (e1, e2))

let expr =
  take_whitespaces
  *> fix (fun all_expr ->
    let cur_expr =
      choice
        [ expr_integer
        ; expr_bool
        ; expr_char
        ; expr_string
        ; expr_valname
        ; parenthesis all_expr
        ]
    in
    let cur_expr = parse_list cur_expr <|> cur_expr in
    let cur_expr = get_by_id cur_expr in
    let cur_expr = chainl1 cur_expr (return (fun e1 e2 -> Expr_app (e1, e2))) in
    let cur_expr = unary_op cur_expr in
    let cur_expr = chainl1 cur_expr (mul <|> div) in
    let cur_expr = chainl1 cur_expr (add <|> sub) in
    let cur_expr = chainr1 cur_expr list_cons_op in
    let cur_expr = chainl1 cur_expr (concat_op <|> concat_format_op) in
    let cur_expr = chainl1 cur_expr rel in
    let cur_expr = chainr1 cur_expr and_op in
    let cur_expr = chainr1 cur_expr or_op in
    let cur_expr = parse_tuple cur_expr in
    choice
      [ if_then_else all_expr
      ; chainr1 cur_expr seq_op
      ; let_in all_expr
      ; expr_match all_expr
      ; expr_fun all_expr
      ; cur_expr
      ])
  <* take_whitespaces
;;

let decl =
  keyword "let"
  *> lift3
       (fun rec_flag name expr -> Let_decl (rec_flag, name, expr))
       (option false (whitespace1_keyword "rec" >>| fun _ -> true))
       (take_whitespaces1 *> valname)
       (take_whitespaces *> op_parse_helper "=" *> expr
        <|> take_whitespaces1 *> fun_helper expr "=")
  <* take_whitespaces
  <* option "" (string ";;")
;;

let expr_top = expr >>| fun e -> Expr e

let program_parser : program t =
  let empty_decl = many (take_whitespaces *> string ";;") in
  many1 (empty_decl *> (decl <|> expr_top) <* empty_decl) <* take_whitespaces
;;

let run_parser_program s = parse_string ~consume:All program_parser s
let run_parser_expr s = parse_string ~consume:All expr s
