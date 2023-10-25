#load "ast.cmi"
#require "angstrom"
open Angstrom
open Ast

let chars2string chars = List.fold_left (fun a b -> a ^ Char.escaped b) "" chars
let brek a = "{" ^ a ^ "}" (*For Debug*)

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
  "else";
  "return"
];;
let is_keyword ch = List.mem ch keywords;;

let rec return_eq_element el_for_comp trans = function
  | a :: tail -> if el_for_comp = trans a then Some a else return_eq_element el_for_comp trans tail
  | _ -> None

(* let is_next_kwd_pars = 
  let rec self = function
  | a :: tail -> peek_string @@ String.length a + 1 >>= (fun str -> 
    match (return_eq_element (str) (fun b -> a ^ Char.escaped b) spaces) with
      | Some _ -> return true
      | _ -> self tail)
  | _ -> return false in
  self keywords *)

let is_spec_symbol = function
  | '$' | '_' -> true
  | _ -> false
;;

let is_valid_first_identifier_ch ch = 
    is_letter ch || is_spec_symbol ch

let is_valid_identifier_ch ch =
  is_valid_first_identifier_ch ch || is_digit ch

let read_word = take_while is_valid_identifier_ch

let is_next_kwd = 
  let rec self = function
  | a :: tail -> string a *> satisfy is_empty *> return true <|> self tail
  | _ -> return false in
  (self keywords >>= fun c ->
    if c then fail "" else return false)
  <|> return true

let empty = take_while(is_empty)
let empty1 = take_while1(is_empty)
let spaces = take_while(is_space)
let token_space s = spaces *> s
let token s = empty *> s
let token1 s = empty1 *> s
let token_str s = token @@ string s
let token_end_of_stm_exc ?(exp = "") s = 
  token ((is_next_kwd >>= (fun c -> if c then fail exp else nothing))
  *> (take_while is_end >>= (function | "" -> nothing | _ -> fail exp))
  *> s)

let to_end_of_stm = 
  empty >>= (fun chs -> 
    end_of_input 
    <|> skip is_end 
    <|> (is_next_kwd >>= (fun c -> if c && (String.exists is_line_break chs) then nothing else fail "incorrect end of statment")))

let is_false_fail cond ?(error_msg="") input = if cond input then return input else fail error_msg

let between p l r= l *> p <* r
let lp = token_str "("
let rp = token_str ")"
let parens p = between p lp rp

let number n = Number n
let const c = Const c

let parse_number = 
  consumed @@ lift3 (fun a b c -> a^b^c) (take_while is_digit) (string ".") (take_while is_digit) 
  <|> take_while1 is_digit >>= (function |"." -> fail "incorrect number" | _ as num -> return num)
  >>| (fun n -> number @@ float_of_string n)
(*TODO: -,NaN..., BigINT*)
let my_parse = parse_number

let bop op first second = BinOp(op, first, second)
let uop op = UnrecognizedOp(op)

let mul_div_rem_op = [("*", Mul); ("/", Div)] (*precedence 12*)
let add_sub_op = [("+", Add); ("-", Sub)] (*precedence 11*)
let equality_op = [("==", Equal); ("!=", NotEqual)] (*precedence 8*)

let list_of_ops = [mul_div_rem_op; add_sub_op; equality_op]

let op_parse ops =
  choice
    (List.map (fun (js_op, op) -> string js_op *> (return op)) ops)

let all_op_parser = op_parse @@ List.concat list_of_ops

let for_every_op func list_for_analizing = 
  let rec parse last_result = function
  | a :: tail -> parse (func (List.map (fun (_, op) -> op) a) [] last_result) tail
  | [] -> last_result in
  parse list_for_analizing list_of_ops

let parse_list_of_mini_expressions parsed_list =
  let rec analize_bin_op ops head = function
    | a :: b :: c :: tail -> 
      (let cur_op = return_eq_element b uop ops in
      match cur_op with
      | Some op -> analize_bin_op ops head ((bop op a c) :: tail)
      | None -> analize_bin_op ops (head @ [a]) (b :: c :: tail))
    | _ as a -> head @ a in
    (*TODO: think how to replace @ operator and mb rewrite all expr parser*)
  match for_every_op analize_bin_op parsed_list with
  | [a] -> a
  | _ as a -> DebugExp a (*For Debug*)
(*TODO: error*)

let parse_expression = 
  fix(fun self ->
    many(token_end_of_stm_exc (choice [
      parens self;
      all_op_parser >>| (uop);
      parse_number >>| const
    ])) >>| parse_list_of_mini_expressions)
   <?> "incorrect expression"
(*TODO: correct next statement recognise and stop ("let a = 3 + 4 \n 5 + 6") (scan?)*)

let valid_identifier =
  token @@ lift2 (^) (satisfy is_valid_first_identifier_ch >>| Char.escaped)
  (fix(fun self -> lift2 (^) (satisfy is_valid_identifier_ch >>| Char.escaped) self <|> return ""))
  (*TODO: Error, Here is some problem with it*)
  
let var_parser (init_word: string) = 
  valid_identifier
  >>= (fun identifier ->
    (token_str "=" *> parse_expression <* to_end_of_stm >>| (fun c -> Some c)) 
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

let parse_word_in words =
  choice (List.map (fun word -> string word <* empty1) words)

let parse_statements stopper = 
  many_till (token (read_word <* empty1 >>= 
    (fun word -> match word with
      | "let" | "const" -> var_parser word
      | _ -> fail "incorrect statement"
    ) 
    (* <|> (many any_char >>| (fun a -> DebugStm (chars2string a))) For Debug *)
     <* empty))
    stopper

let parse_programm = 
  parse_statements end_of_input >>| (fun c -> Programm c)

let parse str =
  match
    Angstrom.parse_string (parse_programm) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`ParsingError er)
;;