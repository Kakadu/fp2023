(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Modules *)
open Angstrom
open Ast

(* Taken names of expression & statements *)
let is_banned = function
  | "and"
  | "or"
  | "true"
  | "false"
  | "return"
  | "if"
  | "else"
  | "while"
  | "def"
  | "class"
  | "lambda" -> true
  | _ -> false
;;

(* Checkers *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_sign = function
  | '-' -> true
  | _ -> false
;;

let is_valid_func_first_char = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_eol = function
  | '\n' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_stmt_sep = function
  | '\n' | ';' | ' ' | '\t' -> true
  | _ -> false
;;

let is_variable = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_quotes = function
  | '\"' -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Skippers *)
let skip_whitespace = skip_while is_whitespace
let between t1 t2 e1 = t1 *> skip_whitespace *> e1 <* skip_whitespace <* t2
let round_brackets e1 = between (string "(") (string ")") e1
let skip_stmt_sep = skip_while is_stmt_sep

(* Takers *)
let take_number = take_while is_digit
let take_string = take_till is_quotes
let take_variable = take_while is_variable
let take_sign = take_while is_sign
let token t = skip_whitespace *> string t
let t_return = token "return"
let t_def = token "def"
let t_mul = token "*"
let t_eq = token "=="
let t_quote = token "\""
let t_div = token "/"
let t_assign = token "="
let t_mod = token "%"
let t_comma = token ","
let t_sub = token "-"
let t_add = token "+"
let t_if = token "if"
let t_else = token "else"
let t_while = token "while"

(*Builders*)
let exp_add e1 e2 = ArithOp (Add, e1, e2)
let exp_sub e1 e2 = ArithOp (Sub, e1, e2)
let exp_mul e1 e2 = ArithOp (Mul, e1, e2)
let exp_div e1 e2 = ArithOp (Div, e1, e2)
let exp_mod e1 e2 = ArithOp (Mod, e1, e2)
let exp_eq e1 e2 = BoolOp (Equal, e1, e2)
let exp_eq e1 e2 = BoolOp (Equal, e1, e2)
let exp_func_call i el = FunctionCall (i, el)
let stmt_expression e = Expression e
let stmt_func i el sl = Function (i, el, sl)
let stmt_if_else e sl1 sl2 = IfElse (e, sl1, sl2)
let stmt_return e = Return e
let stmt_assign e1 e2 = Assign (e1, e2)
let stmt_while e sl = While (e, sl) 

(* Lifters *)
let lift_func_call = lift2 exp_func_call
let lift_return = lift stmt_return
let lift_func = lift3 stmt_func
let lift_assign = lift2 stmt_assign
let lift_if_else = lift3 stmt_if_else
let lift_expression =  lift stmt_expression
let lift_while = lift2 stmt_while

(* Parsers *)
let p_mul = t_mul *> return exp_mul
let p_div = t_div *> return exp_div
let p_mod = t_mod *> return exp_mod
let p_sub = t_sub *> return exp_sub
let p_add = t_add *> return exp_add
let p_assign el1 = lift_assign (el1 <* t_assign) el1
let p_eq = t_eq *> return exp_eq

let p_integer =
  take_sign
  >>= fun sign ->
  take_number >>= fun x -> return ( Const (Int (int_of_string (sign ^ x))) )
;;

let p_string = t_quote *> take_string <* t_quote >>= fun x -> return (Const (String x))

let p_variable =
  take_variable
  >>= function
  | x when not (is_banned x) -> return ( Variable (Identifier x) )
  | _ -> fail "can't name a variable with a taken word"
;;

let p_if_else el sl =
  let exp = t_if *> skip_whitespace *> el <* char ':' <* skip_stmt_sep in
  lift_if_else
    exp
    (sl <* skip_stmt_sep)
    (t_else *> char ':' *> sl <* skip_stmt_sep <|> return [])
;;

let p_identifier =
  skip_whitespace *> take_variable
  <|> skip_whitespace *> take_variable
  >>= function
  | x when not (is_banned x) -> return (Identifier x)
  | _ -> fail "can't name a variable with a taken word"
;;

let p_func_call el = lift_func_call p_identifier (round_brackets el)

let p_return exp =
  let e = t_return *> skip_whitespace *> exp in
  lift_return e
;;

let p_func el sl =
  let id = t_def *> skip_whitespace *> p_identifier <* skip_stmt_sep in
  lift_func
    id
    (round_brackets el <* char ':' <* skip_stmt_sep <|> return [])
    (sl <* skip_stmt_sep)
;;

let p_while e sl =
  let exp = t_while *> skip_whitespace *> e <* char ':' <* skip_stmt_sep in
  lift_while exp sl <* skip_stmt_sep

(* WIP *)
let p_lambda el sl = fail "WIP"

(* Multiple parsers *)
let mp_high_pr_op = p_mul <|> p_div <|> p_mod
let mp_low_pr_op = p_add <|> p_sub
let mp_bool_op = p_eq

type dispatch =
  { p_expression : dispatch -> expression t
  ; p_statement : dispatch -> statement t
  }
let p_exp_or_stmt =
  let p_expression exp_or_stmt =
    fix (fun p_expression ->
      let expression_list = sep_by t_comma p_expression in
      let statement_list = sep_by skip_stmt_sep (exp_or_stmt.p_statement exp_or_stmt) in
      let identifier_list = sep_by t_comma p_identifier in
      let next_char =
        skip_whitespace *> peek_char_fail
        >>= fun c ->
        match c with
        | c when is_valid_func_first_char c ->
              p_func_call expression_list
          <|> p_lambda identifier_list statement_list
          <|> p_variable
        | c when is_digit c || is_sign c -> p_integer
        | '(' -> round_brackets p_expression
        | '\"' -> p_string
        | _ -> fail "unexpected input"
      in
      List.fold_left chainl1 next_char [ mp_high_pr_op; mp_low_pr_op; mp_bool_op ])
  in
  let p_statement exp_or_stmt =
    fix (fun p_statement ->
      let statement_list = sep_by skip_stmt_sep p_statement in
      let identifier_list = sep_by t_comma p_identifier in
      let ps_expression = lift stmt_expression (exp_or_stmt.p_expression exp_or_stmt) in
      let ps_assign = p_assign (exp_or_stmt.p_expression exp_or_stmt) in
      let ps_return = p_return (exp_or_stmt.p_expression exp_or_stmt) in
      let ps_if_else = p_if_else (exp_or_stmt.p_expression exp_or_stmt) statement_list in
      let ps_func = p_func identifier_list statement_list in
      let ps_while = p_while (exp_or_stmt.p_expression exp_or_stmt) statement_list in
      skip_stmt_sep
      *> (ps_func <|> ps_assign <|> ps_return <|> ps_if_else <|> ps_expression <|> ps_while))
  in
  { p_expression; p_statement }
;;

(* function that runs the given parser on a string *)
let parse p s = parse_string ~consume:All p s

(* A parser for Python *)
let pyParser = sep_by skip_stmt_sep (p_exp_or_stmt.p_statement p_exp_or_stmt)

(* Tests *)
let%test _ =
  parse pyParser "print(\"Hello World\")"
  = Ok
      [ Expression
          (FunctionCall (Identifier "print", [ Const (String "Hello World")]))
      ]
;;

let%test _ = parse pyParser "1" = Ok [ Expression (Const (Int 1)) ]

let%test _ =
  parse pyParser "if y == 3:\n  x = 0"
  = Ok
      [ IfElse
          (BoolOp (Equal, Variable(Identifier "y"), Const(Int 3)), [ Assign (Variable (Identifier "x"), Const (Int 0)) ], [])
      ]
;;

let%test _ =
  parse pyParser "myFunction(x)"
  = Ok
      [ Expression
          (FunctionCall (Identifier "myFunction", [ Variable (Identifier "x") ]))
      ]
;;

let%test _ =
  parse pyParser "def testFunction(x):\n    x = 1\n    return x + 1"
  = Ok
      [ Function
          ( Identifier "testFunction"
          , [ Identifier "x" ]
          , [ Assign (Variable (Identifier "x"), Const (Int 1))
            ; Return (ArithOp (Add, Variable (Identifier "x"), Const (Int 1)))
            ] )
      ]
;;

let%test _ =
  parse pyParser "while (y == 2):\n    x = 2"
  = Ok
      [ While
          (BoolOp (Equal, Variable(Identifier "y"), Const(Int 2)), [ Assign (Variable (Identifier "x"), Const (Int 2)) ])
      ]
;;


(*Deadline test*)

let%test _ =
  parse
    pyParser
    "\n\
     def factorial(x):\n\
    \    if (x == 1):\n\
    \        return 1\n\
    \    else:\n\
    \        return (x * factorial(x - 1))"
  = Ok
      [ Function
          ( Identifier "factorial"
          , [ Identifier "x" ]
          , [ IfElse
                ( BoolOp (Equal, Variable (Identifier "x"), Const (Int 1))
                , [ Return (Const (Int 1)) ]
                , [ Return
                      (ArithOp
                         ( Mul
                         , Variable (Identifier "x")
                         , FunctionCall
                             ( Identifier "factorial"
                             , [ ArithOp
                                   (Sub, Variable (Identifier "x"), Const (Int 1))
                               ] ) ))
                  ] )
            ] )
      ]
;;
