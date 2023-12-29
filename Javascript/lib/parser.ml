(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

type error = [ `ParsingError of string ]

let chars2string chars = List.fold_left (fun a b -> a ^ Char.escaped b) "" chars

let nothing = return ()

let is_space = function 
  | ' '
  | '\t' -> true
  | _ -> false
;;

let is_line_break = function
  | '\n'
  | '\r' -> true
  | _ -> false
;;

let is_empty ch = is_space ch || is_line_break ch;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter ch = is_lower ch || is_upper ch;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_end = function
  | ';' -> true
  | _ -> false
;;

let keywords = [
  "let";
  "const";
  "function";
  "if";
  "return";
  "else"
]

let is_keyword ch = List.mem ch keywords;;

let rec return_eq_element el_for_comp trans = function
  | a :: tail -> 
    if el_for_comp = trans a 
      then Some a 
      else return_eq_element el_for_comp trans tail
  | _ -> None

let is_spec_symbol = function
  | '$' | '_' -> true
  | _ -> false
;;

let is_valid_first_identifier_ch ch = 
    is_letter ch || is_spec_symbol ch

let is_valid_identifier_ch ch =
  is_valid_first_identifier_ch ch || is_digit ch

let read_word = take_while is_valid_identifier_ch

let next_is_kwd = 
  let rec self = function
  | a :: tail -> string a *> satisfy is_empty *> return true <|> self tail
  | _ -> return false in
  (self keywords >>= fun c ->
    if c then fail "" else return false)
  <|> return true

let empty = take_while(is_empty)
let empty1 = take_while1(is_empty)
let spaces = take_while(is_space)
let token_space p = spaces *> p
let token p = empty *> p
let token1 p = empty1 *> p
let token_ch ch = token @@ char ch *> return ()
let token_str s = token @@ string s
let token_end_of_stm_exc ?(exp = "") s = 
  token ((next_is_kwd >>= (fun c -> if c then fail exp else nothing))
  *> (take_while is_end >>= (function | "" -> nothing | _ -> fail exp))
  *> s)

let between p l r= l *> p <* r
let lp = token_ch '('
let rp = token_ch ')'
let lc = token_ch '{'
let rc = token_ch '}'
let parens p = between p lp rp
let cur_parens p = between p lc rc

let to_end_of_stm = 
  empty >>= (fun chs -> 
    end_of_input 
    <|> skip is_end 
    <|> (peek_char_fail >>= function | '}' -> nothing | _ -> fail "")
    <|> (next_is_kwd >>= (fun c -> 
      if c && (String.exists is_line_break chs) 
      then nothing else fail "incorrect end of statement")))

let is_false_fail cond ?(error_msg="") input = 
  if cond input 
    then return input 
    else fail error_msg

let some n = Some n
let number n = Number n
let const c = Const c
let var v = Var v
let expression e = Expression e
let fun_call name args = FunctionCall(name, args)

let parse_number = 
  lift3 (fun a b c -> a^b^c) (take_while is_digit) (string ".") (take_while is_digit) 
  <|> take_while1 is_digit >>= (function |"." -> fail "incorrect number" | _ as num -> return num)
  >>| (fun n -> number @@ float_of_string n)
(*TODO: -,NaN..., BigINT*)

let parse_str = 
  char '"' *> take_till (fun c -> c = '"') >>| (fun s -> String s)

let valid_identifier =
  token @@ lift2 (^) 
  (satisfy is_valid_first_identifier_ch >>| Char.escaped <?> "invalid first char of var name")
  (fix(fun self -> 
    lift2 (^) (satisfy is_valid_identifier_ch >>| Char.escaped) self <|> return "") <?> "invalid chars of var name")
  >>= fun name -> 
    if is_keyword name then fail "name of keyword shouldn't be a keyword" else return name
  (*TODO: Error, Here is some problem with it*)


let bop op first second = BinOp(op, first, second)
let uop op = UnrecognizedOp(op)

let mul_div_rem_op = [("*", Mul); ("/", Div)] (*precedence 12*)
let add_sub_op = [("+", Add); ("-", Sub)] (*precedence 11*)
let equality_op = [("==", Equal); ("!=", NotEqual)] (*precedence 8*)

let list_of_bops = [equality_op; add_sub_op; mul_div_rem_op] (*from lower to greater precedence*)

let op_parse ops =
  choice
    (List.map (fun (js_op, op) -> string js_op *> (return op)) ops)

let chainl1 parser op =
  let rec go acc = (token op >>| fun op -> Some op) <|> return None >>= function
  | Some f -> parser >>| (fun x -> BinOp(f, acc, x)) >>= go
  | _ -> return acc in
  parser >>= fun init -> go init

let rec parse_arguments = fun () ->
  parens(sep_by (token_str ",") (expression_parser ())) <?> "incorrect function arguments"

and bop_parser = function
  | a :: b -> chainl1 (bop_parser b) (op_parse a)
  | _ -> mini_expression_parser ()

and mini_expression_parser = fun () ->
  token (choice [
      parens @@ expression_parser ();
      lift2 fun_call valid_identifier (parse_arguments ());
      parse_number >>| const;
      parse_str >>| const;
      valid_identifier >>| var
    ]) <?> "invalid part of expression"

and expression_parser = fun () ->
  fix(fun _ -> bop_parser list_of_bops)
  <?> "incorrect expression"
  
let var_parser (init_word: string) = 
  valid_identifier
  >>= (fun identifier ->
    (token_str "=" *> expression_parser () <* to_end_of_stm >>| some) 
    <|> (to_end_of_stm *> return None) <?> "incorrect definition"
    >>| (fun expr -> VarDeck 
    {
      var_identifier = identifier;
      is_const = init_word = "const";
      var_type = VarType;
      value = expr;
    })
  )  
(*TODO: var support*)

let parse_return =
  token @@ expression_parser () >>| (fun c -> Return c) <* to_end_of_stm

let parse_empty_stm =
  to_end_of_stm >>| fun _ -> EmptyStm

let rec func_parser = fun () ->
  token valid_identifier >>= fun name -> 
    token @@ parse_arguments () >>= fun arguments -> 
      (to_end_of_stm >>| fun _ -> None) <|>
      (lc *> parse_statements rc >>| (fun c -> Some(Block c)) <* to_end_of_stm)
      >>| fun body -> FunDeck { 
          fun_identifier = name; 
          arguments = arguments; 
          body = body 
        }

and parse_block_or_stm = fun () ->
  (lc *> parse_statements rc >>| fun c -> Block c)
  <|> parse_stm ()

and if_parser = fun () ->
  token @@ parens (expression_parser ()) >>= fun condition ->
   parse_block_or_stm () <?> "invalid then statement" >>= fun then_stm ->
    ((token_str "else" *> token1(parse_block_or_stm () >>| some)) <|> return None) <?> "invalid else statement"
     >>| fun else_stm ->
      If (condition, then_stm, else_stm)

and parse_stm = fun () ->
  token (
    parse_empty_stm <|>
    (expression_parser () >>| expression) <|>
    (read_word >>= 
    (fun word -> match word with
      | "let" | "const" -> token1 @@ var_parser word <?> "wrong var statement"
      | "function" -> token1 @@ func_parser () <?> "wrong function statement"
      | "if" -> token1 @@ if_parser () <?> "wrong if statement"
      | "return" -> token parse_return <?> "wrong return statement"
      | "" ->  peek_char_fail >>= fun ch -> fail @@ "there is unexpected symbol: '"^(Char.escaped ch)^"'"
      | _ -> fail @@ "there is an invalid keyword: \""^word^"\""
    ))) <* empty <?> "incorrect statement"

and parse_statements stopper =
    many_till (parse_stm ()) stopper

let parse_programm = 
  parse_statements end_of_input >>| (fun c -> Programm c)

let parse_str ?(parser=parse_programm) str =
  match
    Angstrom.parse_string (parser) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`ParsingError er)
;;

let parse str = parse_str ~parser:parse_programm str

let parse_expression str = parse_str ~parser:(expression_parser () >>| fun e -> Expression e) str