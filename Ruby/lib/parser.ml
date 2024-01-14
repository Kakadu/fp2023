(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ast
open Angstrom

(* Expression *)
let const_value v = Const v
let var modifier id = Var (modifier, id)
let plus e1 e2 = Plus (e1, e2)
let minus e1 e2 = Minus (e1, e2)
let equal e1 e2 = Equal (e1, e2)
let notEqual e1 e2 = NotEqual (e1, e2)
let lessOrEqual e1 e2 = LessOrEqual (e1, e2)
let less e1 e2 = Less (e1, e2)
let greaterOrEqual e1 e2 = GreaterOrEqual (e1, e2)
let greater e1 e2 = Greater (e1, e2)
let mult e1 e2 = Mult (e1, e2)
let div e1 e2 = Div (e1, e2)
let funcMonoCall id exprs = FuncMonoCall (id, exprs)
let funcPolyCall id1 id2 exprs = FuncPolyCall (id1, id2, exprs)
let listAccess id expr = ListAccess (id, expr)
let modOp e1 e2 = ModOp (e1, e2)
let andOp e1 e2 = AndOp (e1, e2)
let orOp e1 e2 = OrOp (e1, e2)
let callLambda args stmts exprs = CallLambda (args, stmts, exprs)

(* Statement *)
let assign expr1 expr2 = Assign (expr1, expr2)
let multiAssign exprs1 exprs2 = MultiAssign (exprs1, exprs2)
let expr expr = Expr expr
let func id args stmts = Func (id, args, stmts)
let ifElse expr stmts1 stmts2 = IfElse (expr, stmts1, stmts2)
let returns expr = Return expr
let continue = Continue
let break = Break
let puts expr = Puts expr
let whileLoop expr stmts = While (expr, stmts)
let classDef id stmts = Class (id, stmts)

(* Id *)
let id id = Id id

(* Value *)
let int n = Int n
let float f = Float f
let bool b = Bool b
let str s = Str s
let null = Const Null
let obj id = Object id
let lambda args stmts = Const (Lambda (args, stmts))
let listExpr exprs = Const (ListExpr exprs)

let is_whitespace = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_seporator = function
  | ' ' | '\t' | '\n' | ';' -> true
  | _ -> false
;;

let is_allowed_first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '$' | '@' | '_' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_signed = function
  | '-' -> true
  | _ -> false
;;

let is_dot = function
  | '.' -> true
  | _ -> false
;;

let is_quote = function
  | '"' -> true
  | _ -> false
;;

let is_variable = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_reserved = function
  | "and"
  | "or"
  | "true"
  | "false"
  | "return"
  | "if"
  | "end"
  | "else"
  | "while"
  | "def"
  | "class"
  | "lambda"
  | "break"
  | "next"
  | "nil"
  | "puts"
  | "new" -> true
  | _ -> false
;;

let skip_whitespace = skip_while is_whitespace
let skip_seporator = skip_while is_seporator
let between t1 t2 a = t1 *> skip_whitespace *> a <* skip_whitespace <* t2
let round_brackets a = between (string "(") (string ")") a
let curly_brackets a = between (string "{") (string "}") a
let square_brackets a = between (string "[") (string "]") a
let vertical_bars a = between (string "|") (string "|") a
let take_number = take_while is_digit
let take_sign = take_while is_signed
let take_dot = take_while1 is_dot
let take_string = take_till is_quote
let take_variable = take_while is_variable

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let plus_parse = skip_whitespace *> string "+" *> return plus
let minus_parse = skip_whitespace *> string "-" *> return minus
let mult_parse = skip_whitespace *> string "*" *> return mult
let div_parse = skip_whitespace *> string "/" *> return div
let mod_parse = skip_whitespace *> string "%" *> return modOp
let equal_parse = skip_whitespace *> string "==" *> return equal
let less_or_equal_parse = skip_whitespace *> string "<=" *> return lessOrEqual
let greater_or_equal_parse = skip_whitespace *> string ">=" *> return greaterOrEqual
let less_parse = skip_whitespace *> string "<" *> return less
let greater_parse = skip_whitespace *> string ">" *> return greater
let notequal_parse = skip_whitespace *> string "!=" *> return notEqual

let and_parse =
  (skip_whitespace *> string "&&" <|> skip_whitespace *> string "and") *> return andOp
;;

let or_parse =
  (skip_whitespace *> string "||" <|> skip_whitespace *> string "or") *> return orOp
;;

let null_parse = skip_whitespace *> string "nil" *> return null
let break_parse = skip_whitespace *> string "break" *> return break
let continue_parse = skip_whitespace *> string "next" *> return continue
let true_parse = skip_whitespace *> string "true" *> return (const_value (bool true))
let false_parse = skip_whitespace *> string "false" *> return (const_value (bool false))

let id_parse =
  let* x =
    skip_whitespace *> char '*' *> take_variable <|> skip_whitespace *> take_variable
  in
  match x with
  | x when not (is_reserved x) -> return @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let local_var_parse =
  let* x = take_variable in
  match x with
  | x when not (is_reserved x) -> return @@ var LocalVar @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let instance_var_parse =
  let* x = skip_whitespace *> string "@" *> take_variable in
  match x with
  | x when not (is_reserved x) -> return @@ var InstanceVar @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let global_var_parse =
  let* x = skip_whitespace *> string "$" *> take_variable in
  match x with
  | x when not (is_reserved x) -> return @@ var GlobalVar @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let class_var_parse =
  let* x = skip_whitespace *> string "@@" *> take_variable in
  match x with
  | x when not (is_reserved x) -> return @@ var ClassVar @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let integer_parse =
  let e sign whole = const_value @@ int @@ int_of_string (sign ^ whole) in
  lift2 e take_sign take_number
;;

let float_parse =
  let e sign whole dot fraction =
    const_value @@ float @@ float_of_string (((sign ^ whole) ^ dot) ^ fraction)
  in
  lift4 e take_sign take_number take_dot take_number
;;

let string_parse =
  let* x =
    skip_whitespace *> string "\"" *> take_string <* skip_whitespace *> string "\""
  in
  return @@ const_value @@ str x
;;

let obj_parse =
  let e id_ _ = const_value @@ obj id_ in
  lift2 e id_parse (take_dot *> skip_whitespace *> string "new")
;;

(* let func_call_parse el1 = (lift2 funccall) id_parse (round_brackets el1) *)
let func_mono_cal_parse el1 = lift2 funcMonoCall id_parse (round_brackets el1)

let func_poly_call_parse el1 =
  lift3 funcPolyCall id_parse (take_dot *> id_parse) (round_brackets el1)
;;

let list_access_parse i1 e1 = lift2 listAccess i1 (square_brackets e1)

let func_parse el1 sl1 =
  let i =
    skip_whitespace *> string "def" *> skip_whitespace *> id_parse <* skip_seporator
  in
  lift3
    func
    i
    (round_brackets el1 <* skip_seporator <|> return [])
    (sl1 <* skip_seporator <* skip_whitespace *> string "end")
;;

let if_else_parse el1 sl1 =
  let e = skip_whitespace *> string "if" *> skip_whitespace *> el1 <* skip_seporator in
  (lift3 ifElse)
    e
    (sl1 <* skip_seporator)
    (skip_whitespace *> string "else" *> sl1
     <* skip_seporator
     <|> return []
     <* skip_whitespace *> string "end")
;;

let return_parse el1 =
  let e =
    skip_whitespace *> string "return" *> skip_whitespace *> (el1 <|> return null)
  in
  (lift returns) e
;;

let puts_parse el1 =
  let e = skip_whitespace *> string "puts" *> skip_whitespace *> (el1 <|> return null) in
  (lift puts) e
;;

let class_parse sl1 =
  let i =
    skip_whitespace *> string "class" *> skip_whitespace *> id_parse <* skip_seporator
  in
  lift2 classDef i sl1 <* skip_seporator <* skip_whitespace *> string "end"
;;

let while_parse el1 sl1 =
  let e = skip_whitespace *> string "while" *> skip_whitespace *> el1 <* skip_seporator in
  lift2 whileLoop e sl1 <* skip_seporator <* skip_whitespace *> string "end"
;;

let lambda_parse el1 sl1 =
  skip_whitespace
  *> string "lambda"
  *> skip_whitespace
  *> curly_brackets
       ((lift2 lambda) (vertical_bars el1) (sl1 <* skip_seporator <|> return []))
;;

let call_lambda_parse i1 sl1 el1 =
  skip_whitespace
  *> string "lambda"
  *> skip_whitespace
  *> string "{"
  *> skip_whitespace
  *> (lift3 callLambda)
       (vertical_bars i1)
       (sl1 <* skip_seporator <|> return [])
       (string "}" *> take_dot *> string "call" *> round_brackets el1)
;;

let assign_parse el1 = (lift2 assign) (el1 <* skip_whitespace *> string "=") el1

let multiple_assign_parse el1 =
  lift2 multiAssign (el1 <* skip_whitespace *> string "=") el1
;;

let expression_parse statement_parse =
  fix
  @@ fun expression_parse ->
  let statement_list = sep_by skip_seporator statement_parse in
  let expression_list = sep_by (skip_whitespace *> string ",") expression_parse in
  let id_list = sep_by (skip_whitespace *> string ",") id_parse in
  let parser =
    let* x = skip_whitespace *> peek_char_fail in
    match x with
    | x when is_allowed_first_letter x ->
      choice
        [ func_mono_cal_parse expression_list
        ; func_poly_call_parse expression_list
        ; obj_parse
        ; list_access_parse id_parse expression_parse
        ; call_lambda_parse id_list statement_list expression_list
        ; lambda_parse id_list statement_list
        ; class_var_parse
        ; global_var_parse
        ; instance_var_parse
        ; local_var_parse
        ; true_parse
        ; false_parse
        ; null_parse
        ]
    | x when is_digit x || is_signed x -> float_parse <|> integer_parse
    | '\"' -> string_parse
    | '(' -> round_brackets expression_parse
    | '[' -> square_brackets expression_list >>| listExpr
    | _ -> fail "unsupported expression is given"
  in
  List.fold_left
    chainl1
    parser
    [ mult_parse
    ; div_parse
    ; mod_parse
    ; minus_parse
    ; plus_parse
    ; equal_parse
    ; notequal_parse
    ; greater_or_equal_parse
    ; less_or_equal_parse
    ; greater_parse
    ; less_parse
    ; and_parse
    ; or_parse
    ]
;;

let statement_parse =
  fix
  @@ fun statement_parse ->
  let expression_list =
    sep_by (skip_whitespace *> string ",") (expression_parse statement_parse)
  in
  let statement_list = sep_by skip_seporator statement_parse in
  let id_list = sep_by (skip_whitespace *> string ",") id_parse in
  let ps_expression = expression_parse statement_parse >>| expr in
  let ps_assign = assign_parse (expression_parse statement_parse) in
  let ps_multi_assign = multiple_assign_parse expression_list in
  let ps_return = return_parse (expression_parse statement_parse) in
  let ps_puts = puts_parse (expression_parse statement_parse) in
  let ps_if_else = if_else_parse (expression_parse statement_parse) statement_list in
  let ps_while = while_parse (expression_parse statement_parse) statement_list in
  let ps_class = class_parse statement_list in
  let ps_func = func_parse id_list statement_list in
  skip_seporator
  *> choice
       [ ps_assign
       ; ps_return
       ; ps_puts
       ; break_parse
       ; continue_parse
       ; ps_if_else
       ; ps_while
       ; ps_class
       ; ps_func
       ; ps_multi_assign
       ; ps_expression
       ]
;;

let final_parse = sep_by skip_seporator statement_parse <* skip_seporator
let parse p s = parse_string ~consume:Prefix p s

let parser_result_to_stmt_list str =
  match parse final_parse str with
  | Ok final_parse -> final_parse
  | Error _ -> []
;;

let interpret_parse p show str =
  match parse p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (show ast)
;;
