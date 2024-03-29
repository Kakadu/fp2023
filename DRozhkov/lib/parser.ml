(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let ptoken p = skip_while is_space *> p
let skip_whitespace = take_while is_space
let token s = spaces *> string s
let staples p = token "(" *> p <* token ")"
let sqbracket p = token "[" *> p <* token "]"

let keywords = function
  | "match" | "with" | "let" | "true" | "false" | "if" | "then" | "else" | "rec" | "in" ->
    true
  | _ -> false
;;

let digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let pnumber =
  ptoken @@ take_while1 digit >>= fun x -> return (PConst (Int (Int.of_string x)))
;;

let pbools =
  skip_whitespace *> choice [ token "true" *> return true; token "false" *> return false ]
  >>= fun x -> return (PConst (Bool x))
;;

let pconst = choice [ pnumber; pbools ]

let number =
  ptoken @@ take_while1 digit >>= fun x -> return (EConst (Int (Int.of_string x)))
;;

let bools =
  skip_whitespace *> choice [ token "true" *> return true; token "false" *> return false ]
  >>= fun x -> return (EConst (Bool x))
;;

let const = choice [ number; bools ]
let plus = token "+" *> return Plus
let min = token "-" *> return Minus
let mult = token "*" *> return Mult
let split = token "/" *> return Split
let less = token "<" *> return Less
let leeq = token "<=" *> return LeEq
let more = token ">" *> return More
let moeq = token ">=" *> return MoEq
let equally = token "=" *> return Equally
let nequally = token "<>" *> return NEqually
let orr = token "||" *> return Or
let andd = token "&&" *> return And

let low = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let identifier =
  skip_whitespace *> peek_char
  >>= function
  | Some x when low x ->
    take_while low
    >>= fun a -> if keywords a then fail "Identifier matches a keyword" else return a
  | _ -> fail "Invalid first char"
;;

let vars = identifier >>| fun x -> Var x
let pvars = identifier >>| fun x -> PVar x

let bundle pexpr =
  let rec fun_bundle pexpr =
    identifier
    >>= fun id -> fun_bundle pexpr <|> token "=" *> pexpr >>| fun e -> EFun (id, e)
  in
  token "let"
  *> lift4
       (fun r id e1 e2 -> ELet (r, id, e1, e2))
       (token "rec" *> return Rec <|> return NoRec)
       (ptoken (token "()") <|> identifier)
       (ptoken (token "=") *> pexpr <|> fun_bundle pexpr)
       (token "in" *> pexpr >>| (fun x -> Some x) <|> return None)
;;

let conditions arg =
  ptoken
    (lift3
       (fun i t e -> EIfThenElse (i, t, e))
       (token "if" *> arg)
       (token "then" *> arg)
       (token "else" *> arg ))
;;

let lists arg = sqbracket (sep_by (token ";") arg >>| fun x -> EList x)

let chainl1 e op =
  let rec go a = lift2 (fun f x -> f a x) op e >>= go <|> return a in
  e >>= fun init -> go init
;;

let patterns =
  fix (fun x ->
    let pattern =
      choice
        [ staples x
        ; pconst
        ; pvars
        ; (token "_" >>| fun _ -> PDash)
        ; (token "[]" >>| fun _ -> PConst Nil)
        ]
    in
    let pattern =
      lift2
        (fun p -> function
          | h :: _ -> PList (p, h)
          | _ -> p)
        pattern
        (many (token "::" *> pattern))
    in
    pattern)
;;

let pebinop chain1 e pbinop = chain1 e (pbinop >>| fun op e1 e2 -> EBinop (e1, op, e2))
let plbinop = pebinop chainl1

let matching pexpr =
  let pcase patterns pexpr =
    lift2 (fun p e -> p, e) (token "|" *> patterns) (token "->" *> pexpr)
  in
  lift2
    (fun expr cases -> EMatch (expr, cases))
    (token "match" *> pexpr <* token "with")
    (many1 (pcase patterns pexpr))
;;

let pexpr =
  fix (fun pexpr ->
    let exp = choice [ staples pexpr; const; vars; lists pexpr; matching pexpr ] in
    let apply =
      lift2
        (fun f args -> List.fold_left ~f:(fun f arg -> EApp (f, arg)) ~init:f args)
        exp
        (many (char ' ' *> ptoken exp))
    in
    let exp = plbinop apply (mult <|> split) in
    let exp = plbinop exp (plus <|> min) in
    let exp =
      plbinop exp (choice [ less; moeq; more; equally; nequally; leeq; orr; andd ])
    in
    choice [ bundle pexpr; exp; conditions pexpr ])
;;

let parse = parse_string ~consume:Consume.All (many1 (bundle pexpr) <* skip_whitespace)
