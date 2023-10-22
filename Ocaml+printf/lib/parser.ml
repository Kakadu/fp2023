open Stdlib
open Angstrom
open Ast

(* for intline testing, debugging *)
let test_parser p input expected =
  let result = parse_string p ~consume:All input in
  match result with
  | Ok actual ->
    let res1 = equal_program expected actual in
    if res1 = false
    then Format.printf "Expected: %a\nActual: %a\n" pp_program expected pp_program actual;
    res1
  | Error msg ->
    Format.printf "Parse error %s\n" msg;
    false
;;

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

let is_keyword = function
  | "let" | "rec" -> true
  | _ -> false
;;

let take_whitespaces = take_while is_whitespace
let take_whitespaces1 = take_while1 is_whitespace

let valname_parser =
  let parse_name = take_while (fun c -> is_letter c || is_digit c || 'c' = '_') in
  any_char
  >>= (function
        | c when is_low_letter c || c = '_' -> return c
        | _ -> fail "expected value")
  >>= fun c -> parse_name >>| fun s -> LCIdent (Base.Char.to_string c ^ s)
;;

let expr_valname_parser = take_whitespaces *> valname_parser >>| fun x -> Expr_val x
let left_bracket = take_whitespaces *> char '('
let right_bracket = take_whitespaces *> char ')'
let parenthesis p = left_bracket *> take_whitespaces *> p <* right_bracket
let const_integer_parser = take_while1 is_digit >>| int_of_string >>| fun x -> Int x

let expr_integer_parser =
  take_whitespaces *> const_integer_parser >>| fun x -> Expr_const x
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

(* not all chars are checked here, only for miniML *)
let second_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' | '<' -> true
  | _ -> false
;;

(* fails if s with characters after that can be interpreted by Ocaml as user-defined operator
   not all cases are considered, only for miniML *)
let op_parse_helper s =
  string s *> peek_char
  >>= function
  | Some x when second_operator_char x -> fail "unsopported operator"
  | _ -> return ""
;;

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

let rec unary_op_parser expr_parser =
  take_whitespaces *> peek_char
  >>= function
  | Some c when c = '+' || c = '-' ->
    op_parse_helper (Base.String.of_char c)
    *> (take_whitespaces *> parenthesis (unary_op_parser expr_parser)
        <|> unary_op_parser expr_parser)
    >>| fun e -> if c = '+' then Un_op (Un_plus, e) else Un_op (Un_minus, e)
  | _ -> expr_parser
;;

let parse_expr =
  fix (fun expr ->
    let item =
      unary_op_parser (parenthesis expr <|> expr_integer_parser <|> expr_valname_parser)
    in
    chainl1 (chainl1 item (mul <|> div)) (add <|> sub))
;;

let rec decl_fun_parser expr_parser =
  valname_parser
  >>= fun valname ->
  choice
    [ (take_whitespaces *> char '=' *> expr_parser >>| fun expr -> Fun (valname, expr))
    ; (take_whitespaces1 *> decl_fun_parser expr_parser
       >>| fun fun_expr -> Fun (valname, fun_expr))
    ]
;;

let decl_parser expr_parser =
  take_whitespaces
  *> string "let"
  *> lift3
       (fun rec_flag name expr -> Let_decl (rec_flag, name, expr))
       (option
          false
          (take_whitespaces1 *> string "rec" >>| fun s -> String.equal s "rec"))
       (take_whitespaces1 *> valname_parser)
       (take_whitespaces *> op_parse_helper "=" *> expr_parser
        <|> take_whitespaces1 *> decl_fun_parser expr_parser)
  <* take_whitespaces
  <* option "" (string ";;")
;;

let program_parser =
  let empty_decl_parser = many (take_whitespaces *> string ";;") in
  many1 (empty_decl_parser *> decl_parser parse_expr <* empty_decl_parser)
  <* take_whitespaces
;;

(* let%test _ =
   let expexted =
   [ Let_decl (false, LCIdent "a", Bin_op (Add, Expr_const (Int 2), Expr_const (Int 2)))
    ]
   in
   test_parser program_parser "let a= -1/2" expexted
   ;; *)

let parse str =
  (* match
     Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
     with
     | Result.Ok x -> Result.Ok x
     | Error er -> Result.Error (`ParsingError er) *)
  String.concat " " [ "Hello"; str ]
;;
