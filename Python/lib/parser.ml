(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Modules *)
open Angstrom
open Ast

(* Pseudo Ast *)
type pseudo_statement =
  | SpecialStatementWithColumns of int * pseudo_statement
  | StatementWithColumns of int * pseudo_statement
  | Expression of expression
  | Assign of expression * expression
  | Function of identifier * identifier list * pseudo_statement list
  | IfElse of expression * pseudo_statement list * pseudo_statement list
  | Else of pseudo_statement list
  | While of expression * pseudo_statement list
  | For of expression * expression list * pseudo_statement list
  | Class of identifier * pseudo_statement list
  | Return of expression
  | Error

let rec convert_pseudoast_to_ast = function
  | SpecialStatementWithColumns (_, body) -> convert_pseudoast_to_ast body
  | StatementWithColumns (_, body) -> convert_pseudoast_to_ast body
  | Function (identifier, params, body) ->
    Ast.Function (identifier, params, List.map (fun a -> convert_pseudoast_to_ast a) body)
  | Expression exp -> Expression exp
  | Assign (left, right) -> Assign (left, right)
  | IfElse (exp, if_body, else_body) ->
    IfElse
      ( exp
      , List.map (fun x -> convert_pseudoast_to_ast x) if_body
      , List.map (fun x -> convert_pseudoast_to_ast x) else_body )
  | Return x -> Return x
  | Class (exp, body) -> Class (exp, List.map (fun a -> convert_pseudoast_to_ast a) body)
  | For (exp1, exp2, body) ->
    For (exp1, exp2, List.map (fun a -> convert_pseudoast_to_ast a) body)
  | While (exp, body) -> While (exp, List.map (fun a -> convert_pseudoast_to_ast a) body)
  | _ -> Error
;;

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

let is_valid_first_char = function
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

let is_whitespace_or_eol = function
  | ' ' | '\n' -> true
  | _ -> false
;;

let is_class = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_curlyLeft = function
  | '{' -> true
  | _ -> false
;;

let is_curlyRight = function
  | '}' -> true
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
let square_brackets e1 = between (string "[") (string "]") e1
let skip_stmt_sep = skip_while is_stmt_sep
(* Takers*)

let take_number = take_while is_digit
let take_string = take_till is_quotes
let take_variable = take_while is_variable
let take_sign = take_while is_sign
let token t = skip_whitespace *> string t
let take_interpolString = take_till is_curlyLeft
let take_interpolIdentifier = take_till is_curlyRight
let t_return = token "return"
let t_def = token "def"
let t_mul = token "*"
let t_eq = token "=="
let t_assign = token "="
let t_not_eq = token "!="
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
let t_for = token "for"
let t_in = token "in"
let t_range = token "range"
let t_and = token "and"
let t_greater = token ">"
let t_less_equal = token "<="
let t_greater_equal = token ">="
let t_less = token "<"
let t_column = token ":"
let t_dot = token "."
let t_or = token "or"
let t_self = token "self."
let t_class = token "class"
let t_lambda = token "lambda"
let t_eol = token "\n"
let t_f = token "f"
let t_curlyLeft = token "{"
let t_curlyRight = token "}"
let t_tab1 = token "\t"

(* Builders *)
let exp_add e1 e2 = ArithOp (Add, e1, e2)
let exp_sub e1 e2 = ArithOp (Sub, e1, e2)
let exp_mul e1 e2 = ArithOp (Mul, e1, e2)
let exp_div e1 e2 = ArithOp (Div, e1, e2)
let exp_mod e1 e2 = ArithOp (Mod, e1, e2)
let exp_eq e1 e2 = BoolOp (Equal, e1, e2)
let exp_not_eq e1 e2 = BoolOp (NotEqual, e1, e2)
let stmt_expression e = Expression e
let expression_with_columns columns expr = StatementWithColumns (columns, Expression expr)
let stmt_func i el sl columns = SpecialStatementWithColumns (columns, Function (i, el, sl))
let stmt_if_else e sl1 sl2 = IfElse (e, sl1, sl2)
let stmt_return e = Return e
let stmt_assign e1 e2 = Assign (e1, e2)
let stmt_while e sl = While (e, sl)
let exp_func_call i el = FunctionCall (i, el)
let exp_and e1 e2 = BoolOp (And, e1, e2)
let exp_or e1 e2 = BoolOp (Or, e1, e2)
let exp_greater e1 e2 = BoolOp (Greater, e1, e2)
let exp_less e1 e2 = BoolOp (Less, e1, e2)
let exp_greater_equal e1 e2 = BoolOp (GreaterOrEqual, e1, e2)
let exp_less_equal e1 e2 = BoolOp (LessOrEqual, e1, e2)

(* Lifters *)
let lift_func_call = lift2 exp_func_call

(* Parsers *)
let p_mul = t_mul *> return exp_mul
let p_div = t_div *> return exp_div
let p_mod = t_mod *> return exp_mod
let p_sub = t_sub *> return exp_sub
let p_add = t_add *> return exp_add
let p_eq = t_eq *> return exp_eq
let p_not_eq = t_not_eq *> return exp_not_eq
let p_and = t_and *> return exp_and
let p_or = t_or *> return exp_or
let p_greater = t_greater *> return exp_greater
let p_gr_eq = t_greater_equal *> return exp_greater_equal
let p_less = t_less *> return exp_less
let p_less_eq = t_less_equal *> return exp_less_equal

let p_integer =
  let* sign = take_sign in
  let* x = take_number in
  return (Const (Int (int_of_string (sign ^ x))))
;;

let p_string =
  let* x = t_quote *> take_string <* t_quote in
  return (Const (String x))
;;

let p_global_variable =
  skip_whitespace *> take_variable
  >>= function
  | x when not (is_banned x) -> return (Variable (Global, Identifier x))
  | _ -> fail "can't name a variable with a taken word"
;;

let p_class_variable =
  t_self *> take_variable
  >>= function
  | x when not (is_banned x) -> return (Variable (Class, Identifier x))
  | _ -> fail "can't name a variable with a taken word"
;;

let p_if el columns =
  let* exp = t_if *> skip_whitespace *> el <* char ':' in
  return (SpecialStatementWithColumns (columns, IfElse (exp, [], [])))
;;

let p_else columns =
  t_else *> t_column *> return (SpecialStatementWithColumns (columns, Else []))
;;

let p_identifier =
  skip_whitespace *> peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_variable
    >>= (function
     | x when not (is_banned x) -> return (Identifier x) <* skip_whitespace
     | _ -> fail "can't name a variable with a taken word")
  | _ -> fail "banned first character"
;;

let p_identifiers = sep_by t_comma p_identifier
let p_func_call el = lift_func_call p_identifier (round_brackets el)

let p_return e columns =
  let* exp = t_return *> e in
  return (StatementWithColumns (columns, Return exp))
;;

let p_func columns =
  let* identifier = t_def *> p_identifier in
  let* arguments = token "(" *> p_identifiers in
  token ")"
  *> t_column
  *> return (SpecialStatementWithColumns (columns, Function (identifier, arguments, [])))
;;

let p_while expr columns =
  let* guard = t_while *> expr <* t_column in
  return (SpecialStatementWithColumns (columns, While (guard, [])))
;;

let p_for e columns =
  let* iterator = t_for *> p_global_variable in
  let* range = t_in *> t_range *> round_brackets (sep_by t_comma e) <* t_column in
  return (SpecialStatementWithColumns (columns, For (iterator, range, [])))
;;

let p_class columns =
  let* identifier = t_class *> p_identifier <* t_column in
  return (SpecialStatementWithColumns (columns, Class (identifier, [])))
;;

let p_object el =
  let* character = peek_char_fail in
  match is_class character with
  | true ->
    let* identifier = p_identifier in
    let* params = round_brackets (sep_by t_comma el) in
    return (Object (identifier, params))
  | false -> fail "couldn't parse an object"
;;

let p_field =
  let* identfier = p_identifier in
  let* field = t_dot *> p_identifier in
  return (Field (identfier, field))
;;

let p_method_call el =
  let* objectIdentifier = p_identifier in
  let* methodIdentifier = t_dot *> p_identifier in
  let* arguments =
    round_brackets
      (sep_by
         (take_while is_whitespace_or_eol *> t_comma <* take_while is_whitespace_or_eol)
         el)
  in
  return (MethodCall (objectIdentifier, methodIdentifier, arguments))
;;

let p_assign e columns =
  let* left = choice [ p_class_variable; p_field; p_global_variable ] in
  let* right = t_assign *> skip_whitespace *> e in
  return (StatementWithColumns (columns, Assign (left, right)))
;;

let anon_func e =
  let* vars = t_lambda *> skip_whitespace *> p_identifiers in
  let* body = t_column *> e in
  return (Lambda (vars, body))
;;

let p_interpolationStrElemEndingWithCurly =
  let* x = take_interpolString <* t_curlyLeft in
  return @@ Str (String x)
;;

let p_interpolationVarElem =
  let* x = take_interpolIdentifier <* t_curlyRight in
  return @@ Var (Identifier x)
;;

let p_interpolationStrElemLast =
  let* x = take_string <* t_quote in
  return @@ Str (String x)
;;

let p_fString =
  let* elems =
    t_f
    *> t_quote
    *> many
         (p_interpolationStrElemEndingWithCurly
          <|> p_interpolationVarElem
          <|> p_interpolationStrElemLast)
  in
  return @@ FString elems
;;

let p_list exp =
  let* list = square_brackets (sep_by (t_comma *> skip_whitespace) exp) in
  return (List list)
;;

(* Multiple parsers *)
let mp_high_pr_op = choice [ p_mul; p_div; p_mod ]
let mp_low_pr_op = choice [ p_add; p_sub ]
let gp_comparison_ops = choice [ p_eq; p_not_eq; p_gr_eq; p_less_eq; p_greater; p_less ]
let gp_logic_ops = choice [ p_and; p_or ]

let optionUnpacker = function
  | Some c -> c
  | _ -> fail ""
;;

(* Main parsers *)

type dispatch =
  { p_expression : dispatch -> expression t
  ; p_statement : dispatch -> pseudo_statement t
  }

let p_exp_or_stmt =
  let p_expression exp_or_stmt =
    fix (fun p_expression ->
      let expression_list = sep_by t_comma p_expression in
      let exp =
        take_while is_whitespace_or_eol *> peek_char_fail
        >>= fun ch1 ->
        peek_char_fail
        >>= fun ch2 ->
        match ch1 with
        | x when x = 'f' & ch2 = '\"' -> p_fString
        | x when is_valid_first_char x ->
          choice
            [ p_class_variable
            ; p_object p_expression
            ; p_method_call p_expression
            ; p_field
            ; p_func_call expression_list
            ; p_global_variable
            ; anon_func p_expression
            ]
        | x when is_digit x -> p_integer
        | '\"' -> p_string
        | '[' -> p_list p_expression
        | '(' -> round_brackets p_expression
        | _ -> fail "unsupported expression"
      in
      List.fold_left
        chainl1
        exp
        [ mp_high_pr_op; mp_low_pr_op; gp_comparison_ops; gp_logic_ops ])
  in
  let p_statement exp_or_stmt =
    fix (fun p_statement ->
      let* count_columns = many (string "\t" <|> string "    ") in
      let columns = List.length count_columns in
      p_assign (p_expression exp_or_stmt) columns
      <|> p_if (p_expression exp_or_stmt) columns
      <|> p_while (p_expression exp_or_stmt) columns
      <|> p_for (p_expression exp_or_stmt) columns
      <|> p_func columns
      <|> p_return (p_expression exp_or_stmt) columns
      <|> p_class columns
      <|> p_else columns
      <|> (p_expression exp_or_stmt >>| expression_with_columns columns))
  in
  { p_expression; p_statement }
;;

let extract_body = function
  | Else body -> body
  | IfElse (_, body, _) -> body
  | For (_, _, body) -> body
  | Function (_, _, body) -> body
  | Class (_, body) -> body
  | While (_, body) -> body
  | _ -> []
;;

let insert_body body = function
  | Else _ -> Else body
  | IfElse (exp, _, else_body) -> IfElse (exp, body, else_body)
  | For (exp1, exp2, _) -> For (exp1, exp2, body)
  | Function (identifier, params, _) -> Function (identifier, params, body)
  | Class (exp, _) -> Class (exp, body)
  | While (exp, _) -> While (exp, body)
  | _ -> Error
;;

let align_pseudo_statement pseudo_statement =
  let empty_body body =
    let rec helper = function
      | [] -> true
      | SpecialStatementWithColumns (_, statements) :: _ ->
        (match extract_body statements with
         | [] -> true
         | _ -> helper (extract_body statements))
      | _ -> false
    in
    helper body
  in
  let rec helper acc = function
    | [] -> return acc
    | StatementWithColumns (columns1, body1) :: tail ->
      (match acc with
       | [] -> helper (StatementWithColumns (columns1, body1) :: acc) tail
       | h :: tl ->
         (match h with
          | StatementWithColumns (columns2, _) when columns2 = columns1 ->
            helper (StatementWithColumns (columns1, body1) :: acc) tail
          | SpecialStatementWithColumns (columns2, body2) ->
            if columns2 >= columns1
            then (
              match empty_body (extract_body body2) with
              | true -> fail "empty statement list"
              | _ -> helper (StatementWithColumns (columns1, body1) :: acc) tail)
            else
              let* new_statements =
                helper (extract_body body2) [ StatementWithColumns (columns1, body1) ]
              in
              helper
                (SpecialStatementWithColumns (columns2, insert_body new_statements body2)
                 :: tl)
                tail
          | _ -> fail "unsupported order of statemtns"))
    | SpecialStatementWithColumns (columns1, body1) :: tail ->
      (match acc with
       | [] -> helper (SpecialStatementWithColumns (columns1, body1) :: acc) tail
       | h :: tl ->
         (match h with
          | StatementWithColumns (columns2, _) ->
            if columns2 < columns1
            then fail "unsupported order of statemtns"
            else helper (SpecialStatementWithColumns (columns1, body1) :: acc) tail
          | SpecialStatementWithColumns (columns2, body2) ->
            if columns2 > columns1
            then (
              match empty_body (extract_body body2) with
              | true -> fail "empty statement list"
              | _ -> helper (SpecialStatementWithColumns (columns1, body1) :: acc) tail)
            else if columns2 = columns1
            then (
              match empty_body (extract_body body2) with
              | true -> fail "empty statement list"
              | _ -> helper (SpecialStatementWithColumns (columns1, body1) :: acc) tail)
            else
              let* new_statements =
                helper
                  (extract_body body2)
                  [ SpecialStatementWithColumns (columns1, body1) ]
              in
              helper
                (SpecialStatementWithColumns (columns2, insert_body new_statements body2)
                 :: tl)
                tail
          | _ -> fail "unsupported order of statemtns"))
    | _ -> fail "unsupported order of statemtns"
  in
  helper [] pseudo_statement
;;

let remove_columns list =
  let join_if_and_else else_statements = function
    | IfElse (guard, if_statements, else_body) ->
      (match else_body with
       | [] -> return (IfElse (guard, if_statements, else_statements))
       | _ -> fail "an error occured while parsing: couldn't join if and else")
    | _ -> fail "an error occured while parsing: couldn't join if and else"
  in
  let rec helper acc = function
    | [] -> return acc
    | SpecialStatementWithColumns (_, pseudo_statement) :: tl ->
      (match pseudo_statement with
       | IfElse (_, _, _) ->
         (match acc with
          | [] ->
            let* body = helper [] (extract_body pseudo_statement) in
            helper (insert_body body pseudo_statement :: acc) tl
          | Else else_body :: tail ->
            let* ifELseStatement = join_if_and_else else_body pseudo_statement in
            helper (ifELseStatement :: tail) tl
          | _ ->
            let* body = helper [] (extract_body pseudo_statement) in
            helper (insert_body body pseudo_statement :: acc) tl)
       | _ ->
         let* body = helper [] (extract_body pseudo_statement) in
         helper (insert_body body pseudo_statement :: acc) tl)
    | StatementWithColumns (_, pseudo_statement) :: tl ->
      helper (pseudo_statement :: acc) tl
    | _ -> return []
  in
  let* pseudo_ast = helper [] list in
  return (List.map (fun x -> convert_pseudoast_to_ast x) pseudo_ast)
;;

(* Final parsers *)
let parse p s = parse_string ~consume:All p s

let pyParser =
  let* pseudo_statement =
    take_while (fun c -> is_eol c)
    *> sep_by t_eol (p_exp_or_stmt.p_statement p_exp_or_stmt)
  in
  let* intermediate_ast = align_pseudo_statement pseudo_statement in
  remove_columns intermediate_ast
;;

let parser s = parse pyParser s
