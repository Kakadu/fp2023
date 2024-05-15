open Ast
open Angstrom

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
  | "def" | "if" | "else" | "then" | "end" | "puts" | "return" | "break" | "next" -> true
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
let equal_parse = skip_whitespace *> string "==" *> return equal
let less_or_equal_parse = skip_whitespace *> string "<=" *> return lessorequal
let notequal_parse = skip_whitespace *> string "!=" *> return notequal
let null_parse = skip_whitespace *> string "nil" *> return (const null)
let break_parse = skip_whitespace *> string "break" *> return break
let continue_parse = skip_whitespace *> string "next" *> return continue
let true_parse = skip_whitespace *> string "true" *> return (const (bool true))
let false_parse = skip_whitespace *> string "false" *> return (const (bool false))

let id_parse =
  let* x =
    skip_whitespace *> char '*' *> take_variable <|> skip_whitespace *> take_variable
  in
  match x with
  | x when not (is_reserved x) -> return @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let var_parse =
  let* x = take_variable in
  match x with
  | x when not (is_reserved x) -> return @@ var @@ id x
  | _ -> fail "variables with reserved words are prohibited"
;;

let integer_parse =
  let e sign whole = const @@ int @@ int_of_string (sign ^ whole) in
  lift2 e take_sign take_number
;;

let float_parse =
  let e sign whole dot fraction =
    const @@ float @@ float_of_string (((sign ^ whole) ^ dot) ^ fraction)
  in
  lift4 e take_sign take_number take_dot take_number
;;

let string_parse =
  let* x =
    skip_whitespace *> string "\"" *> take_string <* skip_whitespace *> string "\""
  in
  return @@ const @@ str x
;;

let func_call_parse el1 = (lift2 funccall) id_parse (round_brackets el1)

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
  (lift3 ifelse)
    e
    (sl1 <* skip_seporator)
    (skip_whitespace *> string "else" *> sl1
     <* skip_seporator
     <|> return []
     <* skip_whitespace *> string "end")
;;

let return_parse el1 =
  let e =
    skip_whitespace *> string "return" *> skip_whitespace *> (el1 <|> return (const null))
  in
  (lift returns) e
;;

let puts_parse el1 =
  let e =
    skip_whitespace *> string "puts" *> skip_whitespace *> (el1 <|> return (const null))
  in
  (lift puts) e
;;

let assign_parse el1 = (lift2 assign) (el1 <* skip_whitespace *> string "=") el1

let expression_parse =
  fix
  @@ fun expression_parse ->
  let expression_list = sep_by (skip_whitespace *> string ",") expression_parse in
  let parser =
    let* x = skip_whitespace *> peek_char_fail in
    match x with
    | x when is_allowed_first_letter x ->
      choice
        [ func_call_parse expression_list
        ; true_parse
        ; false_parse
        ; null_parse
        ; var_parse
        ]
    | x when is_digit x || is_signed x -> float_parse <|> integer_parse
    | '\"' -> string_parse
    | '(' -> round_brackets expression_parse
    | _ -> fail "unsupported expression is given"
  in
  List.fold_left
    chainl1
    parser
    [ choice
        [ equal_parse
        ; notequal_parse
        ; less_or_equal_parse
        ; div_parse
        ; mult_parse
        ; minus_parse
        ; plus_parse
        ]
    ]
;;

let statement_parse =
  fix
  @@ fun statement_parse ->
  let statement_list = sep_by skip_seporator statement_parse in
  let identifier_list = sep_by (skip_whitespace *> string ",") id_parse in
  let ps_expression = expression_parse >>| expr in
  let ps_assign = assign_parse expression_parse in
  let ps_return = return_parse expression_parse in
  let ps_puts = puts_parse expression_parse in
  let ps_if_else = if_else_parse expression_parse statement_list in
  let ps_func = func_parse identifier_list statement_list in
  skip_seporator
  *> choice
       [ ps_assign
       ; ps_return
       ; ps_puts
       ; break_parse
       ; continue_parse
       ; ps_if_else
       ; ps_func
       ; ps_expression
       ]
;;

let final_parse = sep_by skip_seporator statement_parse <* skip_seporator
let parse p s = parse_string ~consume:All p s

let interpret_parse p show str =
  match parse p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (show ast)
;;
