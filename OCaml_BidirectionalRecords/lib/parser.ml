(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let parse p s = parse_string ~consume:Consume.All p s

(* helpers *)

let is_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "fun"
  | "function"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "type" -> true
  | _ -> false
;;

let pws = take_while Char.is_whitespace
let pstoken s = pws *> string s
let ptoken s = pws *> s
let pparens p = pstoken "(" *> p <* pstoken ")"
let pbrackets p = pstoken "{" *> p <* pstoken "}"
let psqparens p = pstoken "[" *> p <* pstoken "]"

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

(* constants *)

let pint =
  let sign = choice [ pstoken "-"; pstoken "+"; pstoken "" ] in
  let rest = take_while1 Char.is_digit in
  lift2 (fun sign rest -> Int.of_string (sign ^ rest)) sign rest >>| fun s -> Int s
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool x
;;

let pstr = char '"' *> take_till (Char.equal '"') <* char '"' >>| fun x -> String x
let pchr = char '\'' *> any_char <* char '\'' >>| fun x -> Char x
let pnil = pstoken "[]" >>| fun _ -> Nil
let punit = pstoken "()" >>| fun _ -> Unit
let const = choice [ pint; pbool; pstr; punit; pnil ]

(* varname *)

let varname =
  let pfirst =
    satisfy (fun ch -> Char.is_alphanum ch || Char.equal ch '_') >>| Char.escaped
  in
  let prest =
    take_while (fun ch -> Char.is_alphanum ch || Char.is_digit ch || Char.equal ch '_')
  in
  let varname = lift2 (fun x y -> x ^ y) pfirst prest in
  ptoken varname
  >>= fun s ->
  if is_keyword s then fail "Variable name conflicts with a keyword" else return s
;;

let ptint = pstoken "int" *> return TInt
let ptstring = pstoken "string" *> return TString
let ptbool = pstoken "bool" *> return TBool
let unspecified = pstoken "" *> return Unspecified
let pty = choice [ ptint; ptstring; ptbool; unspecified ]

(* typed variable *)

let tvar =
  lift2 (fun name ty -> { name; ty }) varname (pstoken ":" *> pty <|> return Unspecified)
;;

(* patterns *)

let pvar = tvar >>| fun x -> PVar x
let pconst = const >>| fun x -> PConst x
let pany = pstoken "_" >>| fun _ -> PAny

let pptuple pexpr =
  lift2 List.cons pexpr (many1 (pstoken "," *> pexpr)) >>| fun x -> PTuple x
;;

(* records *)

let record = pbrackets (lift2 List.cons tvar (many1 (pstoken ";" *> tvar)))
let precord = pstoken "type" *> varname *> pstoken "=" *> record

(* expressions *)

let peconst = const >>| fun x -> EConst x
let pevar = tvar >>| fun x -> EVar x
let peapp e = chainl1 e (return (fun e1 e2 -> EApp (e1, e2)))

let petuple pexpr =
  lift2 List.cons pexpr (many1 (pstoken "," *> pexpr)) >>| fun x -> ETuple x
;;

let plist pexpr = psqparens (sep_by (pstoken ";") pexpr >>| fun x -> EList x)

let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> EIfThenElse (e1, e2, e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr)
;;

let parsebinop op token =
  pws *> pstoken token *> return (fun e1 e2 -> EBinOp (op, e1, e2))
;;

let mult = parsebinop Mult "*"
let div = parsebinop Div "/"
let add = parsebinop Plus "+"
let sub = parsebinop Minus "-"

let rel =
  choice
    [ parsebinop Eq "="
    ; parsebinop Neq "<>"
    ; parsebinop Lt "<"
    ; parsebinop Ltq "<="
    ; parsebinop Gt ">"
    ; parsebinop Gtq ">="
    ]
;;

let plet pexpr =
  let rec pbody pexpr =
    tvar >>= fun id -> pbody pexpr <|> pstoken "=" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> ELet (r, id, e1, e2))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (pparens tvar <|> tvar)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr <|> return EUnit)
;;

let pexpr =
  fix (fun expr ->
    let expr = choice [ peconst; pevar; pparens expr; plist expr ] in
    let expr = peapp expr <|> expr in
    let expr = chainl1 expr (mult <|> div) in
    let expr = chainl1 expr (add <|> sub) in
    let expr = chainl1 expr rel in
    let expr = petuple expr <|> expr in
    let expr = pbranch expr <|> expr in
    let expr = plet expr <|> expr in
    expr)
;;
