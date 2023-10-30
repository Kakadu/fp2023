(** Copyright 2023 Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

let kws =
  [ "CREATE"
  ; "MATCH"
  ; "WITH"
  ; "WHERE"
  ; "DELETE"
  ; "DETACH"
  ; "MERGE"
  ; "RETURN"
  ; "CREATE"
  ; "REMOVE"
  ; "IS"
  ; "NOT"
  ; "NULL"
  ; "AND"
  ; "OR"
  ; "XOR"
  ; "TRUE"
  ; "FALSE"
  ]
;;

let uc = String.uppercase_ascii

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let skip_spaces p =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
  *> p
;;

let skip_spaces_after p =
  p
  <* skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let parens p = skip_spaces (char '(') *> p <* skip_spaces (char ')')
let braces p = skip_spaces (char '{') *> p <* skip_spaces (char '}')
let sq_brackets p = skip_spaces (char '[') *> p <* skip_spaces (char ']')

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let parse p str =
  match parse_string ~consume:All (skip_spaces_after p) str with
  | Ok v -> v
  | Error msg -> failwith msg
;;

let name =
  let name1 =
    lift2
      (fun s1 s2 -> s1 ^ s2)
      (take_while1 is_letter)
      (take_while (fun c -> is_letter c || is_digit c || c = '_'))
    >>= fun n -> if List.mem (uc n) kws then fail "Name cannot be keyword" else return n
  in
  let name2 =
    lift3
      (fun s1 s2 s3 -> s1 ^ s2 ^ s3)
      (char '`' *> return "`")
      (take_while1 (( <> ) '`'))
      (char '`' *> return "`")
  in
  name2 <|> name1
;;

let liter =
  let ltrue =
    take_while is_letter
    >>= fun l ->
    if uc l <> "TRUE" then fail "Literal TRUE parse fail" else return @@ Liter True
  in
  let lfalse =
    take_while is_letter
    >>= fun l ->
    if uc l <> "FALSE" then fail "Literal FALSE parse fail" else return @@ Liter False
  in
  let lnull =
    take_while is_letter
    >>= fun l ->
    if uc l <> "NULL" then fail "Literal NULL parse fail" else return @@ Liter Null
  in
  skip_spaces @@ choice [ ltrue; lfalse; lnull ]
;;

let const =
  let sign = choice [ char '-' *> return "-"; string "" *> return "" ] in
  let int64 =
    lift2
      (fun sign num -> Int64 (Int64.of_string (sign ^ num)))
      sign
      (take_while1 is_digit)
  in
  let float =
    lift4
      (fun sign int dot fract -> Float (float_of_string (sign ^ int ^ dot ^ fract)))
      sign
      (take_while1 is_digit)
      (char '.' *> return ".")
      (take_while1 is_digit)
  in
  let string =
    let content_while_not c = lift (fun s -> String s) (take_while (( <> ) c)) in
    let string c = char c *> content_while_not c <* char c in
    choice [ string '\"'; string '\'' ]
  in
  skip_spaces @@ lift (fun c -> Const c) (choice [ string; float; int64 ])
;;

let var = skip_spaces @@ lift (fun v -> Var v) name

let property =
  skip_spaces
  @@ lift2
       (fun s1 s2 -> Property (s1, s2))
       name
       (skip_spaces (skip (fun c -> c = '.') *> skip_spaces name))
;;

let uminus = skip_spaces (char '-') *> return (fun e -> Un_op (Minus, e))

let uis_null =
  skip_spaces
    (lift2
       (fun s1 s2 -> s1, s2)
       (take_while is_letter)
       (skip_spaces (take_while is_letter))
     >>= fun (s1, s2) ->
     if (uc s1, uc s2) <> ("IS", "NULL")
     then fail "IS_NULL parse fail"
     else return (fun e -> Un_op (IS_NULL, e)))
;;

let uis_not_null =
  skip_spaces
    (lift3
       (fun s1 s2 s3 -> s1, s2, s3)
       (take_while is_letter)
       (skip_spaces (take_while is_letter))
       (skip_spaces (take_while is_letter))
     >>= fun (s1, s2, s3) ->
     if (uc s1, uc s2, uc s3) <> ("IS", "NOT", "NULL")
     then fail "IS_NOT_NULL parse fail"
     else return (fun e -> Un_op (IS_NOT_NULL, e)))
;;

let unot =
  skip_spaces
    (take_while is_letter
     >>= fun s ->
     if uc s <> "NOT" then fail "NOT parse fail" else return (fun e -> Un_op (NOT, e)))
;;

let bcaret = skip_spaces (char '^') *> return (fun e1 e2 -> Bin_op (Caret, e1, e2))
let basterisk = skip_spaces (char '*') *> return (fun e1 e2 -> Bin_op (Asterisk, e1, e2))
let bslash = skip_spaces (char '/') *> return (fun e1 e2 -> Bin_op (Slash, e1, e2))
let bpercent = skip_spaces (char '%') *> return (fun e1 e2 -> Bin_op (Percent, e1, e2))
let bplus = skip_spaces (char '+') *> return (fun e1 e2 -> Bin_op (Plus, e1, e2))
let bminus = skip_spaces (char '-') *> return (fun e1 e2 -> Bin_op (Minus, e1, e2))

let band =
  skip_spaces
    (take_while is_letter
     >>= fun s ->
     if uc s <> "AND"
     then fail "AND parse fail"
     else return (fun e1 e2 -> Bin_op (AND, e1, e2)))
;;

let bor =
  skip_spaces
    (take_while is_letter
     >>= fun s ->
     if uc s <> "OR"
     then fail "OR parse fail"
     else return (fun e1 e2 -> Bin_op (OR, e1, e2)))
;;

let bxor =
  skip_spaces
    (take_while is_letter
     >>= fun s ->
     if uc s <> "XOR"
     then fail "XOR parse fail"
     else return (fun e1 e2 -> Bin_op (XOR, e1, e2)))
;;

let leq =
  skip_spaces (char '=')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Eq, e) :: ls)
    | _ -> List_op (e1, (Eq, e2) :: []))
;;

let lneq =
  skip_spaces (string "<>")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (NEq, e) :: ls)
    | _ -> List_op (e1, (NEq, e2) :: []))
;;

let lless =
  skip_spaces (char '<')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Less, e) :: ls)
    | _ -> List_op (e1, (Less, e2) :: []))
;;

let lgreater =
  skip_spaces (char '>')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Greater, e) :: ls)
    | _ -> List_op (e1, (Greater, e2) :: []))
;;

let lleq =
  skip_spaces (string "<=")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (LEq, e) :: ls)
    | _ -> List_op (e1, (LEq, e2) :: []))
;;

let lgeq =
  skip_spaces (string ">=")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (GEq, e) :: ls)
    | _ -> List_op (e1, (GEq, e2) :: []))
;;

let expr =
  fix (fun expr ->
    let factor = choice [ parens expr; const; property; var; liter ] in
    let null_or_not_null =
      choice
        [ lift2 (fun x f -> f x) factor uis_null
        ; lift2 (fun x f -> f x) factor uis_not_null
        ; factor
        ]
    in
    let minus = null_or_not_null <|> lift2 (fun f x -> f x) uminus null_or_not_null in
    let caret = chainl1 minus bcaret in
    let asterisk_slash_percent = chainl1 caret (choice [ basterisk; bslash; bpercent ]) in
    let plus_minus = chainl1 asterisk_slash_percent (bplus <|> bminus) in
    let list_op =
      chainr1 plus_minus (choice [ leq; lneq; lleq; lgeq; lless; lgreater ])
    in
    let unot = list_op <|> lift2 (fun f x -> f x) unot list_op in
    let bxor = chainl1 unot bxor in
    let band = chainl1 bxor band in
    let bor = chainl1 band bor in
    bor)
;;

let parse_expr = parse expr
