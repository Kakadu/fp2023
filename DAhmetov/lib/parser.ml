(** Copyright 2023-2024, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

type input = char list

let is_empty = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_Uletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "true"
  | "false"
  | "in"
  | "bool"
  | "string"
  | "int"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_iddent_symbol = function
  | c -> is_digit c || is_letter c || is_Uletter c || Char.equal c '_'
;;

let empty = take_while is_empty
let trim p = empty *> p
let trimchar c = empty *> char c
let trimstr s = trim @@ string s
let token s = empty *> string s
let between p = token "(" *> p <* token ")"
let emptylist = trimstr "[]" *> return true

let pIdent =
  trim
  @@ lift2
       (fun hd tl -> Char.escaped hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_letter ch))
       (take_while (fun ch -> ch = '_' || is_letter ch || is_Uletter ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "keyword reserved"
  else if s = "_"
  then fail "wildcard `_` not supported"
  else return s
;;

(*-------------------------------------------------------------------*)
(* types *)
let pvar = trim @@ take_while is_iddent_symbol
let pstring = trim @@ (token "\"" *> take_while is_iddent_symbol) <* token "\""
let pint = int_of_string <$> trim @@ take_while1 is_digit
let pbool = choice [ token "true" *> return true; token "false" *> return false ]
(* let plist pe = tlist <$> pe <* trimchunk "list" *)

(* Pattern *)
let pnil = token "[]" *> return ppnill

let pptuple pp =
  sep_by (trimstr ",") pp
  >>= function
  | [] -> pp
  | [ h ] -> return h
  | h :: tl -> return (pptuple (h :: tl))
;;

let ppbase ppatern =
  choice
    [ between ppatern
    ; ppint <$> trim pint
    ; ppbool <$> pbool
    ; ppvar <$> pvar
    ; ppstring <$> pstring
    ]
;;

let ppattern =
  fix
  @@ fun ppatern ->
  let pp = ppbase ppatern in
  let pp = pnil <|> pp in
  let pp = pptuple pp <|> pp in
  pp
;;

(* Operation *)
let peMul pe =
  chainl1 pe (trim @@ (token "*" *> return (fun ex1 ex2 -> e_binop Multi ex1 ex2)))
;;

let peDiv pe =
  chainl1 pe (trim @@ (token "/" *> return (fun ex1 ex2 -> e_binop Division ex1 ex2)))
;;

let pePlus pe =
  chainl1 pe (trim @@ (token "+" *> return (fun ex1 ex2 -> e_binop Plus ex1 ex2)))
;;

let peMinus pe =
  chainl1 pe (trim @@ (token "-" *> return (fun ex1 ex2 -> e_binop Minus ex1 ex2)))
;;

let peEq pe =
  chainl1 pe (trim @@ (token "=" *> return (fun ex1 ex2 -> e_binop Equal ex1 ex2)))
;;

let peNotEq pe =
  chainl1 pe (trim @@ (token "<>" *> return (fun ex1 ex2 -> e_binop NotEqual ex1 ex2)))
;;

let peLt pe =
  chainl1 pe (trim @@ (token "<" *> return (fun ex1 ex2 -> e_binop Lt ex1 ex2)))
;;

let peLtq pe =
  chainl1 pe (trim @@ (token "<=" *> return (fun ex1 ex2 -> e_binop Ltq ex1 ex2)))
;;

let peGt pe =
  chainl1 pe (trim @@ (token ">" *> return (fun ex1 ex2 -> e_binop Gt ex1 ex2)))
;;

let peGtq pe =
  chainl1 pe (trim @@ (token ">=" *> return (fun ex1 ex2 -> e_binop Gtq ex1 ex2)))
;;

let peAnd pe =
  chainl1 pe (trim @@ (token "&&" *> return (fun ex1 ex2 -> e_binop And ex1 ex2)))
;;

let peOr pe =
  chainl1 pe (trim @@ (token "||" *> return (fun ex1 ex2 -> e_binop Or ex1 ex2)))
;;

(* Same operation*)

let peApp pe = chainl1 pe (return (fun exp1 exp2 -> e_app exp1 exp2))

let pif pe =
  trim @@ lift3 e_ifthenelse (token "if" *> pe) (token "then" *> pe) (token "else" *> pe)
;;

let pematch expr =
  let first_parse expr =
    lift2 (fun pattern result -> pattern, result) (ppattern <* trimstr "->") expr
  in
  let elem_parse expr =
    lift2
      (fun pattern result -> pattern, result)
      (trimchar '|' *> ppattern <* trimstr "->")
      expr
  in
  lift2
    e_match
    (trimstr "match" *> expr <* trimstr "with")
    (many1 (elem_parse expr)
     <|> (first_parse expr >>= fun h -> many (elem_parse expr) >>| fun tl -> h :: tl))
;;

let petuple pe =
  sep_by (trimstr ",") pe
  >>= function
  | [] -> pe
  | [ h ] -> return h
  | h :: tl -> return (e_tuple (h :: tl))
;;

let pefun pe = trim @@ (token "fun" *> lift2 e_fun ppattern (trim @@ (token "->" *> pe)))

let pelet pe =
  token "let"
  *> lift4
       e_let
       (token "rec" *> return true <|> return false)
       pIdent
       (token "=" *> pe)
       (token "in" *> pe)
;;

let pexp =
  fix
  @@ fun pexp ->
  let pe = between pexp in
  let pe = e_string <$> pstring <|> pe in
  let pe = e_var <$> pIdent <|> pe in
  let pe = e_bool <$> pbool <|> pe in
  let pe = e_int <$> pint <|> pe in
  let pe = peApp pe <|> pe in
  let pe = pePlus pe <|> pe in
  let pe = peMinus pe <|> pe in
  let pe = peMul pe <|> pe in
  let pe = peDiv pe <|> pe in
  let pe = peEq pe <|> pe in
  let pe = peNotEq pe <|> pe in
  let pe = peLt pe <|> pe in
  let pe = peLtq pe <|> pe in
  let pe = peGt pe <|> pe in
  let pe = peGt pe <|> pe in
  let pe = peGtq pe <|> pe in
  let pe = peAnd pe <|> pe in
  let pe = peOr pe <|> pe in
  let pe = petuple pe <|> pe in
  let pe = pif pe <|> pe in
  let pe = pematch pe <|> pe in
  let pe = pefun pe <|> pe in
  let pe = pelet pe <|> pe in
  pe
;;

let pdecl =
  token "let"
  *> lift3 d_decl (token "rec" *> return true <|> return false) pIdent (token "=" *> pexp)
;;

let parse_program = many pdecl <* empty
let parse str = parse_string ~consume:All parse_program str

(*========================================================================================*)
