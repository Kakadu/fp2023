(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

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

let letdecl a b d = a, b, d
let totuple a b = lift2 (fun a b -> a, b) a b

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_wildcard = function
  | '_' -> true
  | _ -> false
;;

let is_lchar = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_char ch = is_lchar ch || is_uchar ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let spaces = take_while is_space
let spaces1 = take_while1 is_space
let pchunk p = spaces *> p
let check_chunk s = pchunk @@ string s
let pparens p = check_chunk "(" *> p <* check_chunk ")"

(* variable and constructor parsers *)

let prstring = check_chunk "\"" *> take_while (fun ch -> ch != '\"') <* check_chunk "\""
let prnumber = int_of_string <$> take_while1 is_digit
let prbool = check_chunk "true" *> return true <|> check_chunk "false" *> return false
let premptylist = check_chunk "[]" *> return true

let prvarname =
  pchunk
    (take_while (fun ch -> is_uchar ch || is_digit ch || is_wildcard ch)
     >>= function
     | "" ->
       take_while1 (fun ch -> is_wildcard ch || is_char ch || is_digit ch)
       >>= fun str -> if is_keyword str then fail "invalid var name" else return str
     | _ -> fail "invalid var name")
;;

let prconstrname =
  pchunk
    (take_while (fun ch -> is_lchar ch || is_digit ch || is_wildcard ch)
     >>= function
     | "" ->
       take_while1 (fun ch -> is_wildcard ch || is_char ch || is_digit ch)
       >>= fun str -> if is_keyword str then fail "invalid constr name" else return str
     | _ -> fail "invalid constr name")
;;

(* type parser *)

let pftbase pftype =
  choice
    [ pparens pftype
    ; check_chunk "int" *> return tint
    ; check_chunk "string" *> return tstring
    ; check_chunk "bool" *> return tbool
    ; tvar <$> (lname <$> prvarname)
    ]
;;

let pftlist pft = tlist <$> pft <* check_chunk "list"

let pfttuple pft =
  sep_by (check_chunk "*") pft
  >>= function
  | [] -> pft
  | [ h ] -> return h
  | h :: tl -> return (ttuple (h :: tl))
;;

let pftfun = check_chunk "->" *> return (fun type1 type2 -> tfun type1 type2)

let pftype =
  fix
  @@ fun pftype ->
  let pft = pftbase pftype in
  let pft = pftlist pft <|> pft in
  let pft = pfttuple pft <|> pft in
  chainr1 pft pftfun
;;

(* pattern parser *)

let ppnill = check_chunk "[]" *> return pnill
let ppwild = (check_chunk "_" <* spaces) *> return pwild
let fcons = check_chunk "::" *> return (fun ptr1 ptr2 -> pcons ptr1 ptr2)
let ppcons pp = chainr1 pp fcons

let ppcase pp =
  fix
  @@ fun ppcase ->
  uname
  <$> prconstrname
  >>= fun name ->
  ppcase <|> (pp >>= fun exp -> return (padt name (Some exp))) <|> return (padt name None)
;;

let pptuple pp =
  sep_by (check_chunk ",") pp
  >>= function
  | [] -> pp
  | [ h ] -> return h
  | h :: tl -> return (ptuple (h :: tl))
;;

let ppbase ppatern =
  choice
    [ pparens ppatern
    ; pint <$> pchunk prnumber
    ; pbool <$> prbool
    ; pvar <$> (lname <$> prvarname)
    ; pstring <$> prstring
    ]
;;

let ppattern =
  fix
  @@ fun ppattern ->
  let pp = ppbase ppattern in
  let pp = ppnill <|> pp in
  let pp = ppwild <|> pp in
  let pp = ppcons pp <|> pp in
  let pp = pptuple pp <|> pp in
  let pp = ppcase pp <|> pp in
  pp
;;

(* expression parser *)

let pfelet pfexp =
  check_chunk "let"
  *> lift3
       letdecl
       (drec <$> (check_chunk "rec" *> return true <|> return false))
       (lname <$> prvarname)
       (check_chunk "=" *> pfexp)
;;

let pfebase pfexp =
  choice
    [ pparens pfexp
    ; eint <$> pchunk prnumber
    ; ebool <$> prbool
    ; evar <$> (lname <$> prvarname)
    ; estring <$> prstring
    ; return eemptylist <* premptylist
    ]
;;

let pfetuple pfexp =
  sep_by (check_chunk ",") pfexp
  >>= function
  | [] -> pfexp
  | [ h ] -> return h
  | h :: tl -> return (etuple (h :: tl))
;;

let pfefun pfe =
  fix @@ fun pfefun -> lift2 efun ppattern (pfefun <|> check_chunk "->" *> pfe)
;;

let pfeif pfexp =
  fix
  @@ fun pfeif ->
  lift3
    eif
    (check_chunk "if" *> (pfeif <|> pfexp))
    (check_chunk "then" *> (pfeif <|> pfexp))
    (check_chunk "else" *> (pfeif <|> pfexp))
;;

let pfematch pfe =
  lift2
    ematch
    (check_chunk "match" *> pfe <* check_chunk "with")
    (many1 @@ totuple (check_chunk "|" *> ppattern <* check_chunk "->") pfe)
;;

let pfeapp pfe = chainl1 pfe (return (fun exp1 exp2 -> eapp exp1 exp2))

let pfeconstr pfe =
  fix
  @@ fun pfeconstr ->
  lift2
    (fun decl_name decl_exp -> econstr decl_name decl_exp)
    (uname <$> prconstrname)
    (pfeconstr <|> pfe >>= (fun exp -> return (Some exp)) <|> return None)
;;

(* parse operators *)

let fmul = check_chunk "*" *> return (fun exp1 exp2 -> ebinop bop_mul exp1 exp2)
let pfemul pfe = chainl1 pfe fmul
let fdiv = check_chunk "/" *> return (fun exp1 exp2 -> ebinop bop_div exp1 exp2)
let pfediv pfe = chainl1 pfe fdiv
let fadd = check_chunk "+" *> return (fun exp1 exp2 -> ebinop bop_add exp1 exp2)
let pfeadd pfe = chainl1 pfe fadd
let fsub = check_chunk "-" *> return (fun exp1 exp2 -> ebinop bop_sub exp1 exp2)
let pfesub pfe = chainl1 pfe fsub
let feq = check_chunk "=" *> return (fun exp1 exp2 -> ebinop bop_eq exp1 exp2)
let pfeeq pfe = chainl1 pfe feq
let fneq = check_chunk "<>" *> return (fun exp1 exp2 -> ebinop bop_neq exp1 exp2)
let pfeneq pfe = chainl1 pfe fneq
let fles = check_chunk "<" *> return (fun exp1 exp2 -> ebinop bop_les exp1 exp2)
let pfeles pfe = chainl1 pfe fles
let fleq = check_chunk "<=" *> return (fun exp1 exp2 -> ebinop bop_leq exp1 exp2)
let pfeleq pfe = chainl1 pfe fleq
let fgre = check_chunk ">" *> return (fun exp1 exp2 -> ebinop bop_gre exp1 exp2)
let pfegre pfe = chainl1 pfe fgre
let fgeq = check_chunk ">=" *> return (fun exp1 exp2 -> ebinop bop_geq exp1 exp2)
let pfegeq pfe = chainl1 pfe fgeq
let fand = check_chunk "&&" *> return (fun exp1 exp2 -> ebinop bop_and exp1 exp2)
let pfeand pfe = chainr1 pfe fand
let ffor = check_chunk "||" *> return (fun exp1 exp2 -> ebinop bop_or exp1 exp2)
let pfeor pfe = chainr1 pfe ffor
let fcons = check_chunk "::" *> return (fun exp1 exp2 -> ebinop bop_cons exp1 exp2)
let pfecons pfe = chainr1 pfe fcons

let pfearith pfe =
  let pfe = pfemul pfe <|> pfe in
  let pfe = pfediv pfe <|> pfe in
  let pfe = pfeadd pfe <|> pfe in
  let pfe = pfesub pfe <|> pfe in
  let pfe = pfecons pfe <|> pfe in
  let pfe = pfeneq pfe <|> pfe in
  let pfe = pfeeq pfe <|> pfe in
  let pfe = pfeles pfe <|> pfe in
  let pfe = pfeleq pfe <|> pfe in
  let pfe = pfegre pfe <|> pfe in
  let pfe = pfegeq pfe <|> pfe in
  pfe
;;

(* doc:https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity *)
let pfexp =
  fix
  @@ fun pfexp ->
  let pfe = pfebase pfexp in
  let pfe = pfeapp pfe <|> pfe in
  let pfe = pfeconstr pfe <|> pfe in
  let pfe = pfearith pfe <|> pfe in
  let pfe = pfeand pfe <|> pfe in
  let pfe = pfeor pfe <|> pfe in
  let pfe = pfetuple pfe <|> pfe in
  let pfe = pfeif pfe <|> pfe in
  let pfe = pfematch pfe <|> pfe in
  let pfe = check_chunk "fun" *> pfefun pfe <|> pfe in
  let pfe = lift2 elet (pfelet pfe) (check_chunk "in" *> pfe) <|> pfe in
  pfe
;;

(* declaration parser *)

let pdecl =
  check_chunk "let"
  *> lift3
       decllet
       (drec <$> (check_chunk "rec" *> return true <|> return false))
       (lname <$> prvarname)
       (check_chunk "=" *> pfexp)
  <|> check_chunk "type"
      *> lift2
           decltype
           (lname <$> prvarname)
           (check_chunk "="
            *> (many1
                @@ totuple
                     (check_chunk "|" *> (uname <$> prconstrname))
                     (dtype <$> (check_chunk "of" *> pftype <|> return temptytype))))
;;

let parse_semicolon = many @@ check_chunk ";;"
let parse_program = parse_semicolon *> many (pdecl <* parse_semicolon) <* spaces
let parse str = parse_string ~consume:All parse_program str
