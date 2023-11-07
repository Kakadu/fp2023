(** Copyright 2021-2023, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

type dispatch =
  { parse_bin_op : dispatch -> expr Angstrom.t
  ; parse_application : dispatch -> expr Angstrom.t
  ; parse_fun : dispatch -> expr Angstrom.t
  ; parse_if_then_else : dispatch -> expr Angstrom.t
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

(* Constructors for binary operations *)
let sAdd _ = Plus
let sSub _ = Sub
let sMul _ = Mul
let sDiv _ = Div
let sEq _ = Eq
let sNEq _ = NEq
let sGt _ = Gt
let sGte _ = Gte
let sLt _ = Lt
let sLte _ = Lte
let sAnd _ = And
let sOr _ = Or
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
let skip_wspace1 = take_while1 is_whitespace

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

let parse_ident =
  skip_wspace *> peek_char
  >>= is_acceptable_fl
  >>= fun _ ->
  take_while is_ident
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: name is used as keyword"
  else return @@ EIdentifier s
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
      ; pack.parse_application pack
      ; self
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
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
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; self
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif_then_else
           parse_expr
           (skip_wspace *> string "then" *> parse_expr)
           (skip_wspace *> string "else" *> parse_expr)
;;

let parse_bin_op pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let addition = skip_wspace *> char '+' >>| sAdd
  and subtraction = skip_wspace *> char '-' >>| sSub
  and multiplication = skip_wspace *> char '*' >>| sMul
  and division = skip_wspace *> char '/' >>| sDiv
  and eqality = skip_wspace *> char '=' >>| sEq
  and neqality = skip_wspace *> string "!=" <|> string "<>" >>| sEq
  and logand = skip_wspace *> string "&&" >>| sAnd
  and logor = skip_wspace *> string "||" >>| sOr
  and larger = skip_wspace *> char '>' >>| sGt
  and largerEq = skip_wspace *> string ">=" >>| sGte
  and less = skip_wspace *> char '<' >>| sLt
  and lessEq = skip_wspace *> string "<=" >>| sLte in
  let parse_expr =
    choice
      [ parens self
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  and chainl1 e op =
    let rec go acc =
      lift2 (fun f x -> EBinaryOperation (f, acc, x)) op e >>= go <|> return acc
    in
    e >>= fun init -> go init
  in
  let ( <||> ) = chainl1 in
  parse_expr
  <||> multiplication
  <||> division
  <||> addition
  <||> subtraction
  <||> larger
  <||> largerEq
  <||> less
  <||> lessEq
  <||> eqality
  <||> neqality
  <||> logand
  <||> logor
  >>= fun s ->
  match s with
  | EBinaryOperation (_, _, _) -> return s
  | _ -> fail "Error: not binary operation."
;;

let parse_declaration pack =
  fix
  @@ fun _ ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  >>= function
  | "rec" ->
    lift3
      rec_declraration
      parse_uncapitalized_name
      (many parse_uncapitalized_name)
      (skip_wspace *> string "=" *> parse_expr)
  | _ ->
    lift3
      declraration
      parse_uncapitalized_name
      (many parse_uncapitalized_name)
      (skip_wspace *> string "=" *> parse_expr)
;;

let parse_application pack =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let function_parser =
        choice
          [ parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_if_then_else pack
          ; parse_ident
          ]
      and operand_parser =
        choice
          [ parens @@ pack.parse_bin_op pack
          ; parens self
          ; parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_if_then_else pack
          ; parse_const
          ; parse_ident
          ]
      in
      let chainl acc = lift (eapplication acc) operand_parser in
      let rec go acc = chainl acc >>= go <|> return acc in
      function_parser >>= fun init -> chainl init >>= fun init -> go init)
;;

let default = { parse_bin_op; parse_application; parse_fun; parse_if_then_else }

let parse input =
  parse_string ~consume:All (many (parse_declaration default) <* skip_wspace) input
;;
