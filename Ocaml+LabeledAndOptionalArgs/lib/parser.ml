(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(*Checks*)

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

let is_func_type = function
  | FuncType (_, _, true) -> true
  | _ -> false
;;

let is_char c = is_char_up c || is_char_lr c
let is_correct_first_letter c = is_char_lr c || is_underscore c
let is_correct_var_name c = is_char c || is_digit c || is_underscore c || is_apostrophe c

(*Auxiliary functions*)

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let remove_spaces = take_while is_space
let remove_between p = remove_spaces *> p <* remove_spaces
let check_word s = remove_spaces *> string s
let check_char c = remove_spaces *> char c
let remove_brackets p = check_char '(' *> p <* check_char ')'

let check_name =
  remove_spaces *> satisfy is_correct_first_letter
  >>= fun c ->
  take_while is_correct_var_name
  >>= fun s ->
  let name = Char.escaped c ^ s in
  if is_keyword name then fail "Invalid var" else return name
;;

let var_check =
  check_name >>= fun s -> if is_keyword s then fail "Invalid var" else return s
;;

let digits_parse =
  remove_between (take_while1 is_digit >>= fun s -> return (Int (Int.of_string s)))
;;

let bool_parse =
  check_word "true" *> return (Bool true) <|> check_word "false" *> return (Bool false)
;;

let char_parse =
  check_char '\'' *> any_char >>= fun c -> return (Char c) <* check_char '\''
;;

let string_parse =
  check_char '\"' *> take_till is_quote >>= fun s -> return (String s) <* check_char '\"'
;;

(*Type parser*)

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

(*Since all functions are considered as functions of a single argument,
  only the first function will have the option flag in ast.
  Internal functions will not have such a label*)

let type_parse =
  fix
  @@ fun p ->
  let t = base_types p in
  let t = list_type t <|> t in
  let t = tuple_type t <|> t in
  fun_type t
;;

(*pattern parse*)

let pconst_parse p =
  choice
    [ remove_brackets p
    ; (digits_parse >>| fun c -> PConst c)
    ; (bool_parse >>| fun c -> PConst c)
    ; (char_parse >>| fun c -> PConst c)
    ; (string_parse >>| fun c -> PConst c)
    ]
;;

let pvar_parse p = remove_brackets p <|> (var_check >>| fun s -> PVar s)
let base_patterns p = pconst_parse p <|> pvar_parse p
let fun_pnil_parse p = remove_brackets p <|> check_word "[]" *> return PNil
let pnil_parse = check_word "[]" *> return PNil
let pempty_parse = check_word "_" *> return PEmpty
let arg_const_parse = choice [ digits_parse; bool_parse; char_parse; string_parse ]

let no_label_arg_parse =
  fix
  @@ fun p ->
  remove_brackets p
  <|> (check_name
       >>= fun n ->
       check_char ':' *> type_parse >>= (fun t -> return (n, t)) <|> return (n, EmptyType)
      )
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
       <|> return (n, sn, EmptyType))
    <|> remove_brackets f
  in
  choice
    [ (check_name
       >>= fun n -> check_char ':' *> check_name >>= fun sn -> return (n, sn, EmptyType))
    ; (check_name >>= fun n -> check_char ':' *> helper n)
    ; remove_brackets
        (check_name >>= fun n -> check_char ':' *> type_parse >>= fun t -> return (n, n, t)
        )
    ; remove_brackets p
    ; (check_name >>= fun n -> return (n, n, EmptyType))
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
         <|> return (n, Some c, EmptyType))
  in
  choice
    [ remove_brackets (check_name >>= fun n -> check_char '=' *> helper n)
    ; remove_brackets
        (check_name
         >>= fun n -> check_char ':' *> type_parse >>= fun t -> return (n, None, t))
    ; remove_brackets p
    ; (check_name >>= fun n -> return (n, None, EmptyType))
    ]
;;

let parg_parse =
  choice
    [ (check_char '~' *> labeled_arg_parse >>| fun (n, sn, t) -> PArg (n, Label sn, t))
    ; (check_char '?' *> optional_arg_parse >>| fun (n, c, t) -> PArg (n, Optional c, t))
    ; (no_label_arg_parse >>| fun (n, t) -> PArg (n, NoLabel, t))
    ]
;;

let cons_parse1 = check_word "::" *> return (fun ptr1 ptr2 -> PCons (ptr1, ptr2))
let pcons_parse p = chainr1 p cons_parse1

let ptuple_parse p =
  sep_by (check_char ',') p
  >>= function
  | [] -> p
  | [ h ] -> return h
  | h :: tl -> return (PTuple (h :: tl))
;;

let pattern_parse =
  fix
  @@ fun p ->
  let pattern = base_patterns p in
  let pattern = pnil_parse <|> pattern in
  let pattern = pempty_parse <|> pattern in
  let pattern = pcons_parse pattern <|> pattern in
  let pattern = ptuple_parse pattern <|> pattern in
  pattern
;;

let fun_pattern_parse =
  fix
  @@ fun p ->
  let pattern = fun_pnil_parse p in
  let pattern = pempty_parse <|> pattern in
  let pattern = parg_parse <|> pattern in
  let pattern = pcons_parse pattern <|> pattern in
  let pattern = ptuple_parse pattern <|> pattern in
  pattern
;;

(*expressions parse*)

let const_parse p =
  choice
    [ remove_brackets p
    ; (digits_parse >>= fun c -> return (Const c))
    ; (bool_parse >>= fun c -> return (Const c))
    ; (char_parse >>= fun c -> return (Const c))
    ; (string_parse >>= fun c -> return (Const c))
    ]
;;

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

let match_parse expr =
  let first_elem_parse expr =
    lift2 (fun a b -> a, b) (pattern_parse <* check_word "->") expr
  in
  let elem_parse expr =
    lift2 (fun a b -> a, b) (check_char '|' *> pattern_parse <* check_word "->") expr
  in
  lift2
    ematch
    (check_word "match" *> expr <* check_word "with")
    (many1 (elem_parse expr)
     <|> (first_elem_parse expr >>= fun h -> many (elem_parse expr) >>| fun tl -> h :: tl)
    )
;;

let fun_parse e =
  let helper e =
    fix
    @@ fun p ->
    lift2
      efun
      fun_pattern_parse
      (p <|> check_char ':' *> type_parse *> check_word "->" *> e <|> check_word "->" *> e)
  in
  check_word "fun" *> helper e
;;

let let_parse expr =
  let is_rec = check_word "rec" *> return true <|> return false in
  let helper expr =
    let rec helper l e =
      match l with
      | h :: tl -> efun h (helper tl e)
      | [] -> e
    in
    many fun_pattern_parse
    >>= fun l ->
    check_char ':' *> type_parse
    >>= (fun t -> check_char '=' *> expr >>| fun e -> helper l e, t)
    <|> (check_char '=' *> expr >>| fun e -> helper l e, EmptyType)
  in
  check_word "let" *> is_rec
  >>= fun b -> check_name >>= fun n -> helper expr >>| fun (e, t) -> let_decl b n e t
;;

let let_in_parse expr = lift2 edecl (let_parse expr) (check_word "in" *> expr)

let expr_parse =
  fix
  @@ fun p ->
  let expr = base_expr p in
  let expr = apply_parse expr <|> expr in
  let expr = bin_op_parse expr <|> expr in
  let expr = if_then_else_parse expr <|> expr in
  let expr = match_parse expr <|> expr in
  let expr = fun_parse expr <|> expr in
  let expr = let_in_parse expr <|> expr in
  expr
;;

let parse_program = many (let_parse expr_parse) <* remove_spaces
let parse str = Angstrom.parse_string parse_program ~consume:Angstrom.Consume.All str
