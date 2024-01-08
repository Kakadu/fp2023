(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_equal = function
  | '=' -> true
  | _ -> false
;;

let is_colon = function
  | ':' -> true
  | _ -> false
;;

let is_underscore = function
  | '_' -> true
  | _ -> false
;;

let is_apostrophe = function
  | '\'' -> true
  | _ -> false
;;

let is_char_lr = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_char_up = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_quote = function
  | '\"' -> true
  | _ -> false
;;

let is_func_type d =
  match d with
  | FuncType (_, _, true) -> true
  | _ -> false
;;

let is_char c = is_char_up c || is_char_lr c
let is_correct_first_letter c = is_char_lr c || is_underscore c
let is_correct_var_name c = is_char c || is_digit c || is_underscore c || is_apostrophe c
let remove_spaces = take_while is_space
let remove_between p = remove_spaces *> p <* remove_spaces
let check_word s = remove_spaces *> string s
let check_char c = remove_spaces *> char c
let remove_brackets p = check_char '(' *> p <* check_char ')'
let remove_lots_of_brackets p = many (check_char '(') *> p <* many (check_char ')')

let digits_parse =
  remove_between (take_while1 is_digit >>= fun s -> return (Int (Int.of_string s)))
;;

let bool_parse =
  check_word "true"
  >>= fun _ -> return (Bool true) <|> (check_word "false" >>= fun _ -> return (Bool false))
;;

let char_parse =
  check_char '\'' *> any_char >>= fun c -> return (Char c) <* check_char '\''
;;

let string_parse =
  check_char '\"' *> take_till is_quote >>= fun s -> return (String s) <* check_char '\"'
;;

let const_parse p =
  choice
    [ remove_brackets p
    ; (digits_parse >>= fun c -> return (Const c))
    ; (bool_parse >>= fun c -> return (Const c))
    ; (char_parse >>= fun c -> return (Const c))
    ; (string_parse >>= fun c -> return (Const c))
    ]
;;

let arg_const_parse = choice [ digits_parse; bool_parse; char_parse; string_parse ]

let check_name =
  remove_between
    (satisfy is_correct_first_letter
     >>= fun c -> take_while is_correct_var_name >>| fun s -> Char.escaped c ^ s)
;;

let var_check =
  check_name >>= fun s -> if is_keyword s then fail "Invalid var" else return s
;;

let base_types t =
  choice
    [ remove_brackets t
    ; check_word "unit" *> check_word "option" *> return (UnitType true)
    ; check_word "int" *> check_word "option" *> return (IntType true)
    ; check_word "bool" *> check_word "option" *> return (BoolType true)
    ; check_word "char" *> check_word "option" *> return (CharType true)
    ; check_word "string" *> check_word "option" *> return (StringType true)
    ; check_word "unit" *> return (UnitType false)
    ; check_word "int" *> return (IntType false)
    ; check_word "bool" *> return (BoolType false)
    ; check_word "char" *> return (CharType false)
    ; check_word "string" *> return (StringType false)
    ]
;;

let list_type arg =
  arg
  >>= fun t ->
  check_word "list" *> check_word "option"
  >>= (fun _ -> return (ListType (t, true)))
  <|> (check_word "list" >>= fun _ -> return (ListType (t, false)))
;;

let tuple_type t =
  sep_by1 (check_char '*') t
  >>= fun l ->
  (check_word "option"
   *>
   match l with
   | [ _ ] -> fail "Isn't tuple_type function responsive"
   | _ -> return (TupleType (l, true)))
  <|>
  match l with
  | [ x ] -> return x
  | _ -> return (TupleType (l, false))
;;

let fun_type t =
  chainr1 t (check_word "->" *> return (fun t1 t2 -> FuncType (t1, t2, true)))
  >>= (fun e ->
        check_word "option"
        *>
        match e with
        | FuncType (t1, t2, _) -> return (FuncType (t1, t2, true))
        | _ -> fail "")
  <|> chainr1 t (check_word "->" *> return (fun t1 t2 -> FuncType (t1, t2, false)))
;;

let type_parse =
  fix
  @@ fun p ->
  let t = base_types p in
  let t = list_type t <|> t in
  let t = tuple_type t <|> t in
  fun_type t
;;

let arg_parse =
  fix
  @@ fun p ->
  remove_brackets p
  <|> (check_name
       >>= fun n ->
       check_char ':' *> type_parse
       >>= (fun t -> return (n, t))
       <|> return (n, UndefinedType))
;;

let labeled_arg_parse =
  fix
  @@ fun p ->
  let helper n =
    fix
    @@ fun f ->
    remove_brackets
      (var_check
       >>= fun sn ->
       check_char ':' *> type_parse
       >>= (fun t -> return (n, sn, t))
       <|> return (n, sn, UndefinedType))
    <|> remove_brackets f
  in
  choice
    [ (check_name
       >>= fun n ->
       check_char ':' *> check_name >>= fun sn -> return (n, sn, UndefinedType))
    ; (check_name >>= fun n -> check_char ':' *> helper n)
    ; remove_brackets
        (check_name >>= fun n -> check_char ':' *> type_parse >>= fun t -> return (n, n, t)
        )
    ; remove_brackets p
    ; (check_name >>= fun n -> return (n, n, UndefinedType))
    ]
;;

let optional_arg_parse =
  fix
  @@ fun p ->
  let helper n =
    fix
    @@ fun f ->
    remove_brackets f
    <|> (arg_const_parse
         >>= fun c ->
         check_char ':' *> type_parse
         >>= (fun t -> return (n, Some c, t))
         <|> return (n, Some c, UndefinedType))
  in
  choice
    [ remove_brackets (check_name >>= fun n -> check_char '=' *> helper n)
    ; remove_brackets
        (check_name
         >>= fun n -> check_char ':' *> type_parse >>= fun t -> return (n, None, t))
    ; remove_brackets p
    ; (check_name >>= fun n -> return (n, None, UndefinedType))
    ]
;;

let check_label =
  remove_spaces
  *> choice
       [ (check_char '~' *> labeled_arg_parse >>| fun (n, sn, t) -> Label (n, sn, t))
       ; (check_char '?' *> optional_arg_parse >>| fun (n, c, t) -> Optional (n, c, t))
       ; (arg_parse >>| fun (n, t) -> NoLabel (n, t))
       ]
;;

let args_parse = many check_label
let var_parse p = remove_brackets p <|> (var_check >>| fun s -> Var s)
let base_expr p = const_parse p <|> var_parse p

let if_then_else_parse p =
  check_word "if" *> p
  >>= fun e1 ->
  check_word "then" *> p
  >>= fun e2 -> check_word "else" *> p >>= fun e3 -> return (IfThenElse (e1, e2, e3))
;;

let dashp e =
  let helper = check_char '-' *> return (fun ex1 ex2 -> BinOp (Dash, ex1, ex2)) in
  chainl1 e helper
;;

let plusp e =
  let helper = check_char '+' *> return (fun ex1 ex2 -> BinOp (Plus, ex1, ex2)) in
  chainl1 e helper
;;

let slashp e =
  let helper = check_char '/' *> return (fun ex1 ex2 -> BinOp (Slash, ex1, ex2)) in
  chainl1 e helper
;;

let eqp e =
  let helper = check_char '=' *> return (fun ex1 ex2 -> BinOp (Eq, ex1, ex2)) in
  chainl1 e helper
;;

let neqp e =
  let helper = check_word "<>" *> return (fun ex1 ex2 -> BinOp (Neq, ex1, ex2)) in
  chainl1 e helper
;;

let andp e =
  let helper = check_word "&&" *> return (fun ex1 ex2 -> BinOp (And, ex1, ex2)) in
  chainr1 e helper
;;

let orp e =
  let helper = check_word "||" *> return (fun ex1 ex2 -> BinOp (Or, ex1, ex2)) in
  chainr1 e helper
;;

let astrp e =
  let helper = check_char '*' *> return (fun ex1 ex2 -> BinOp (Asterisk, ex1, ex2)) in
  chainl1 e helper
;;

let lsp e =
  let helper = check_char '<' *> return (fun ex1 ex2 -> BinOp (Less, ex1, ex2)) in
  chainl1 e helper
;;

let lsqp e =
  let helper = check_word "<=" *> return (fun ex1 ex2 -> BinOp (Lessq, ex1, ex2)) in
  chainl1 e helper
;;

let grp e =
  let helper = check_word ">" *> return (fun ex1 ex2 -> BinOp (Greater, ex1, ex2)) in
  chainl1 e helper
;;

let grqp e =
  let helper = check_word ">=" *> return (fun ex1 ex2 -> BinOp (Greaterq, ex1, ex2)) in
  chainl1 e helper
;;

let consp e =
  let helper = check_word "::" *> return (fun ex1 ex2 -> BinOp (Cons, ex1, ex2)) in
  chainr1 e helper
;;

let bin_op_parse expr =
  let expr = astrp expr <|> expr in
  let expr = slashp expr <|> expr in
  let expr = plusp expr <|> expr in
  let expr = dashp expr <|> expr in
  let expr = consp expr <|> expr in
  let expr = neqp expr <|> expr in
  let expr = eqp expr <|> expr in
  let expr = lsp expr <|> expr in
  let expr = lsqp expr <|> expr in
  let expr = grp expr <|> expr in
  let expr = grqp expr <|> expr in
  let expr = andp expr <|> expr in
  let expr = orp expr <|> expr in
  expr
;;

let apply_parse e = chainl1 e (return (fun e1 e2 -> Apply (e1, e2)))
let fun_decl = check_char ':' *> type_parse <|> return UndefinedType

let fun_parse e =
  check_word "fun" *> args_parse
  >>= fun l ->
  fun_decl >>= fun t -> check_word "->" *> e >>= fun e1 -> return (Fun (l, t, e1))
;;

let let_parse e f =
  check_word "let"
  *> lift4
       f
       (check_word "rec" *> return true <|> return false)
       check_name
       args_parse
       (fun_decl <* check_word "=")
  <*> e
;;

let expr_parse =
  fix
  @@ fun p ->
  let expr = base_expr p in
  let expr = apply_parse expr <|> expr in
  let expr = bin_op_parse expr <|> expr in
  let expr = if_then_else_parse expr <|> expr in
  let expr = fun_parse expr <|> expr in
  let expr = let_parse expr let_edecl <*> check_word "in" *> expr <|> expr in
  expr
;;

let parse_program = many (let_parse expr_parse let_decl) <* remove_spaces
let parse str = Angstrom.parse_string parse_program ~consume:Angstrom.Consume.All str
