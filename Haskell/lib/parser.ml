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
  | "False"
  | "Leaf"
  | "Node" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let pwspaces = take_while is_ws
let pwspaces1 = take_while1 is_ws
let pspaces = take_while (fun c -> is_ws c || is_eol c)
let pspaces1 = take_while1 (fun c -> is_ws c || is_eol c)
let ptoken p = pwspaces *> p
let ptoken1 p = pwspaces1 *> p
let pstoken s = ptoken (string s)
let pstoken1 s = ptoken1 (string s)
let ptoken_eol p = pspaces *> p
let ptoken_eol1 p = pspaces1 *> p
let pstoken_eol s = ptoken_eol (string s)
let pstoken_eol1 s = ptoken_eol1 (string s)
let between l r p = pwspaces *> string l *> p <* pwspaces *> string r
let pparens p = between "(" ")" p
let pbrackets p = between "[" "]" p
let lit_int s = LitInt (int_of_string s)
let lit_bool s = LitBool (bool_of_string @@ String.lowercase_ascii s)
let lit_str s = LitString s
let lit_char s = LitChar s
let expr_var id = ExprVar id
let expr_lit lit = ExprLit lit
let expr_fun args expr = List.fold_right (fun p e -> ExprFunc (p, e)) args expr
let expr_binop op lexp rexp = ExprBinOp (op, lexp, rexp)
let expr_tuple l = ExprTuple l
let expr_app o1 o2 = ExprApp (o1, o2)
let expr_cons e1 e2 = ExprCons (e1, e2)
let expr_list l = List.fold_right (fun e1 e2 -> ExprCons (e1, e2)) l ExprNil
let expr_nil _ = ExprNil
let expr_let dl e = ExprLet (dl, e)
let expr_if e1 e2 e3 = ExprIf (e1, e2, e3)
let expr_case e l = ExprCase (e, l)
let expr_tree t = ExprTree t
let pat_var pat = PatVar pat
let pat_lit lit = PatLit lit
let pat_wild _ = PatWild
let pat_nil _ = PatNil
let pat_cons e1 e2 = PatCons (e1, e2)
let pat_list l = List.fold_right (fun e1 e2 -> PatCons (e1, e2)) l PatNil
let pat_tuple l = PatTuple l
let pat_leaf _ = PatLeaf
let pat_tree v n1 n2 = PatTree (v, n1, n2)
let dec_let pat expr = DeclLet (pat, expr)
let bind p e = p, e
let node e n1 n2 = Node (e, n1, n2)

let pname =
  pwspaces
  *> lift2
       (fun hd tl -> String.make 1 hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_alpha ch))
       (take_while (fun ch ->
          ch = '_' || ch = '\'' || is_alpha ch || is_capital ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: keyword reserved"
  else if s = "_"
  then fail "Parsing error: variable name can't be \"_\""
  else return s
;;

(* Literals *)

let pint = lit_int <$> ptoken @@ take_while1 is_digit
let pbool = lit_bool <$> (pstoken "True" <|> pstoken "False")
let pstring = lit_str <$> ptoken @@ between "\"" "\"" (take_till (Char.equal '"'))
let pchar = lit_char <$> ptoken @@ between "\'" "\'" any_char
let plit = choice [ pint; pbool; pstring; pchar ]

(* Patterns *)

let ppat =
  fix
  @@ fun pattern ->
  let pplit = pat_lit <$> plit in
  let pwild = pat_wild <$> pstoken "_" in
  let ppvar = pat_var <$> pname in
  let ppnil = pat_nil <$> pstoken "[]" in
  let pplist =
    let contents = sep_by (char ',') @@ ptoken pattern in
    pbrackets contents >>| pat_list
  in
  let pcons = pparens @@ chainr1 pattern (pstoken ":" *> return pat_cons) in
  let pptuple =
    let contents = sep_by (char ',') @@ ptoken pattern in
    pparens contents
    >>| function
    | [ x ] -> x
    | elems -> pat_tuple elems
  in
  let ptree =
    let pnil = pat_leaf <$> pstoken "Leaf" in
    let pnode = pparens @@ (pstoken "Node" *> lift3 pat_tree pattern pattern pattern) in
    pnil <|> pnode
  in
  choice
    ~failure_msg:"Parsing error: can't parse pattern"
    [ ppnil; pptuple; pplit; ppvar; pwild; pplist; pcons; ptree ]
;;

(* Expressions *)

let pbinop op op_constr = pwspaces *> string op *> return (expr_binop op_constr)
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
  let pexprlit = expr_lit <$> plit in
  let pexprvar = expr_var <$> pname in
  let pexprtuple =
    let contents = sep_by (char ',') @@ ptoken pexpr in
    pparens contents
    >>= function
    | [ x ] -> return x
    | elems -> return (expr_tuple elems)
  in
  let pneg = char '-' *> (pexprlit <|> pparens pexpr) >>| fun e -> ExprUnOp (Neg, e) in
  let penil = expr_nil <$> pstoken "[]" in
  let pelist = expr_list <$> pbrackets (sep_by (char ',') @@ ptoken pexpr) in
  let pvalue =
    choice [ pexprlit; pexprvar; pneg; pexprtuple; pelist; penil; pparens pexpr ]
  in
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
    let pcons =
      let op = pstoken ":" *> return expr_cons in
      chainr1 paddsub op
    in
    let pcmpr =
      let op = choice [ peq; pneq; pleq; pgeq; plt; pgt ] in
      chainl1 pcons op
    in
    let pandor =
      let op = choice [ pand; por ] in
      chainl1 pcmpr op
    in
    pandor
  in
  let pif =
    lift3
      expr_if
      (pstoken_eol "if" *> pexpr)
      (pstoken_eol "then" *> pexpr)
      (pstoken_eol "else" *> pexpr)
  in
  let plambda =
    pstoken "\\" *> lift2 expr_fun (many ppat) (pstoken "->" *> ptoken pexpr)
  in
  let plocbind =
    let pbinds =
      let pbind =
        lift2
          bind
          (ptoken_eol ppat)
          (lift2 expr_fun (many ppat) (pstoken_eol "=" *> ptoken_eol pexpr))
      in
      pstoken_eol "let"
      *> many1 (ptoken_eol pbind <* ptoken_eol @@ many @@ take_while1 is_eol)
      <* pstoken_eol "in"
      <* pspaces
    in
    lift2 expr_let pbinds pexpr
  in
  let pcase =
    let pbranch =
      lift2 (fun pattern expr -> pattern, expr) (pspaces1 *> ppat) (pstoken "->" *> pexpr)
    in
    lift2 expr_case (pstoken "case" *> pexpr <* pstoken "of") (many pbranch)
  in
  let ptree =
    let pnil = pstoken_eol "Leaf" *> return Leaf in
    let pnode =
      let helper = pvalue <|> pexpr in
      pstoken_eol "Node" *> lift3 node helper helper helper
    in
    expr_tree <$> (pnil <|> pnode)
  in
  let c =
    choice
      ~failure_msg:"Parsing error: can't parse expression"
      [ ptree; pcase; plocbind; pif; pebinop; plambda ]
  in
  c <|> pparens c
;;

let pdecl =
  lift2
    dec_let
    (ptoken_eol ppat)
    (lift2 expr_fun (many ppat) (pstoken "=" *> ptoken_eol pexpr))
  <* pwspaces
;;

let pprog : prog t = many1 (ptoken pdecl <* ptoken @@ many @@ take_while1 is_eol)
let parse s = parse_string ~consume:All pprog s
