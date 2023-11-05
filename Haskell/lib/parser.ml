(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_ws = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_eol = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_capital = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_keyword = function
  | "case"
  | "of"
  | "if"
  | "then"
  | "else"
  | "let"
  | "in"
  | "where"
  | "data"
  | "True"
  | "False" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let pwspaces = take_while is_ws
let pwspaces1 = take_while1 is_ws
let pspaces = take_while (fun c -> is_ws c || is_eol c)
let pspaces1 = take_while1 (fun c -> is_ws c || is_eol c)
let ptoken p = pwspaces *> p
let ptoken1 p = pwspaces1 *> p
let pstoken s = ptoken (string s)
let pstoken1 s = ptoken1 (string s)
let between l r p = pstoken l *> p <* pstoken r
let pparens p = between "(" ")" p
let pbrackets p = between "[" "]" p
let lit_int s = LitInt (int_of_string s)
let lit_str s = LitString s
let lit_char s = LitChar s
let expr_var id = ExprVar id
let expr_lit lit = ExprLit lit
let expr_fun args expr = List.fold_right (fun p e -> ExprFunc (p, e)) args expr
let expr_binop op lexp rexp = ExprBinOp (op, lexp, rexp)
let expr_tuple l = ExprTuple l
let expr_app o1 o2 = ExprApp (o1, o2)
let pat_var pat = PatVar pat
let pat_lit lit = PatLit lit
let pat_wild _ = PatWild
let pat_list l = PatList l
let pat_tuple l = PatTuple l
let dec_let pat expr = DeclLet (pat, expr)

let pname =
  ptoken
  @@ lift2
       (fun hd tl -> String.make 1 hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_alpha ch))
       (take_while (fun ch ->
          ch = '_' || ch = '\'' || is_alpha ch || is_capital ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: keyword reserved"
  else if s = "_"
  then fail "Parsing error: wildcard `_` isn't supported"
  else return s
;;

(* Literals *)

let pint = lit_int <$> ptoken @@ take_while1 is_digit
let pstring = lit_str <$> ptoken @@ (char '"' *> take_till (Char.equal '"')) <* char '"'
let pchar = lit_char <$> ptoken @@ (char '\'' *> any_char) <* char '\''
let plit = choice [ pint; pstring; pchar ]

(* Patterns *)

let ppat =
  fix
  @@ fun pattern ->
  let pplit = pat_lit <$> plit in
  let pwild = pat_wild <$> pstoken "_" in
  let ppvar = pat_var <$> pname in
  let pplist =
    let contents = sep_by (char ',') @@ ptoken pattern in
    pbrackets contents >>| pat_list
  in
  let pptuple =
    let contents = sep_by (char ',') @@ ptoken pattern in
    pparens contents
    >>= function
    | [ x ] -> return x
    | elems -> return (pat_tuple elems)
  in
  choice [ pplist; pptuple; pplit; ppvar; pwild ]
;;

(* Expressions *)

let pbinop op op_constr = pstoken op *> return (expr_binop op_constr)
let padd = pbinop "+" Add
let psub = pbinop "-" Sub
let pmul = pbinop "*" Mul
let pdiv = pbinop "/" Div
let pand = pbinop "&&" And
let por = pbinop "||" Or
let peq = pbinop "==" Eq
let pneq = pbinop "/=" Neq
let plt = pbinop "<" Lt
let pgt = pbinop ">" Gt
let pleq = pbinop "<=" Leq
let pgeq = pbinop ">=" Geq

let pexpr =
  fix
  @@ fun pexpr ->
  let pexprvar = expr_var <$> pname in
  let pexprlit = expr_lit <$> plit in
  let pvalue = choice [ pexprlit; pexprvar; (* pneg; *) pparens pexpr ] in
  let pebinop =
    let app = chainl1 pvalue (return expr_app) in
    let pmuldiv =
      let op = choice [ pmul; pdiv ] in
      chainl1 app op
    in
    let paddsub =
      let op = choice [ padd; psub ] in
      chainl1 pmuldiv op
    in
    let pcmpr =
      let op = choice [ peq; pneq; pleq; pgeq; plt; pgt ] in
      chainl1 paddsub op
    in
    let pandor =
      let op = choice [ pand; por ] in
      chainl1 pcmpr op
    in
    pandor
  in
  let pif =
    pstoken "if" *> pexpr
    <* pstoken1 "then"
    <* pwspaces1
    >>= fun cond ->
    pexpr
    <* pstoken1 "else"
    <* pwspaces1
    >>= fun then_branch ->
    pexpr >>| fun else_branch -> ExprIf (cond, then_branch, else_branch)
  in
  (* let pcase =
     pstoken "case" *> ptoken pexpr
     <* pstoken1 "of"
     <* pspaces1
     >>= fun caseexpr ->
     many1
     (ppat
     <* string "->"
     <* space
     >>= fun pat -> parse_expr <* space >>| fun result_expr -> pat, result_expr)
     >>| fun branches -> Case (case_expr, branches)
     in *)
  choice [ pif; pebinop ]
;;

let pdecl =
  lift2 dec_let (ptoken ppat) (lift2 expr_fun (many ppat) (pstoken "=" *> ptoken pexpr))
  <* pspaces
;;

let pprog : prog t = many1 (ptoken pdecl <* ptoken @@ many @@ pstoken ";;")
let parse s = parse_string ~consume:All pprog s
