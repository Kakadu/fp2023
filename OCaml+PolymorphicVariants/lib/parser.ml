(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_idc c = Char.is_alphanum c || Char.equal c '_'

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while Char.is_whitespace
let token s = ws *> string s
let lp = token "("
let rp = token ")"
let lsb = token "["
let rsb = token "]"
let semi = token ";"
let dsemi = token ";;"
let dcol = token "::"
let comma = token ","
let grd = token "|"
let arr = token "->"
let parens p = lp *> p <* rp
let brackets p = lsb *> p <* rsb

(*--------------------------------- Constants --------------------------------*)

let cint i = CInt i
let cbool b = CBool b
let cstring s = CString s

let pcint =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* num = take_while1 Char.is_digit in
  return @@ Int.of_string (sign ^ num) >>| cint
;;

let pcbool =
  let t = token "true" *> return (cbool true) in
  let f = token "false" *> return (cbool false) in
  choice [ t; f ]
;;

let pcstring =
  token "\""
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* char '"'
  >>| cstring
;;

let pcunit = token "()" *> return CUnit
let pcnil = token "[]" *> return CNil
let const = choice [ pcint; pcbool; pcstring; pcunit; pcnil ]

(*------------------------------ Patterns ------------------------------------*)

let pconst c = PConst c
let pvar x = PVar x
let ppoly t p = PPoly (t, p)
let pcons p1 p2 = PCons (p1, p2)
let ptuple ps = PTuple ps
let ppoly t p = PPoly (t, p)
let ppconst = const >>| pconst
let ppany = token "_" *> return PAny

let varname =
  let* fst =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest = take_while is_idc in
  match String.of_char fst ^ rest with
  | "_" -> fail "Wildcard can't be used as indetifier"
  | s when is_keyword s -> fail "Keyword can't be used as identifier"
  | _ as name -> return name
;;

let tagname =
  let* fst = token "`" *> satisfy Char.is_uppercase in
  let* rest = take_while is_idc in
  return @@ String.of_char fst ^ rest
;;

let ppvar = varname >>| pvar
let pptuple p = lift2 List.cons p (many1 (comma *> p)) >>| ptuple
let pplist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:pcons ~init:(pconst CNil)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

let pppoly (p : pattern t) =
  both (many1 tagname) (option None (p >>| fun p -> Some p))
  >>| fun (tags, pat) ->
  List.fold_right
    (List.drop_last_exn tags)
    ~f:(fun tag pat -> ppoly tag (Some pat))
    ~init:(PPoly (List.last_exn tags, pat))
;;

let pattern =
  fix (fun pat ->
    let term = choice [ ppconst; ppvar; ppany; pplist pat; parens pat ] in
    let poly = pppoly term <|> term in
    let cons = chainr1 poly (dcol *> return pcons) in
    let tuple = pptuple cons <|> cons in
    tuple)
;;

(*----------------------------- Expressions ----------------------------------*)
let econst c = EConst c
let evar x = EVar x
let ebinop op e1 e2 = EBin_op (op, e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let ematch e cl = EMatch (e, cl)
let elet f b e = ELet (f, b, e)
let efun p e = EFun (p, e)
let etuple el = ETuple el
let econs e1 e2 = ECons (e1, e2)
let eapply e1 e2 = EApply (e1, e2)
let epoly t e = EPoly (t, e)
let peconst = const >>| econst
let pevar = varname >>| evar

let peif pe =
  fix (fun peif ->
    lift3
      eif
      (token "if" *> (peif <|> pe))
      (token "then" *> (peif <|> pe))
      (token "else" *> (peif <|> pe)))
;;

let pematch pe =
  let pexpr = token "match" *> pe <* token "with" <* option "" grd in
  let pcase = lift2 (fun p e -> p, e) (pattern <* arr) pe in
  lift2 ematch pexpr (sep_by1 grd pcase)
;;

let pepoly p = lift2 epoly tagname (option None (p >>| fun e -> Some e))
let petuple p = lift2 List.cons p (many1 (comma *> p)) >>| etuple
let pelist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:econs ~init:(econst CNil)
let efunf ps e = List.fold_right ps ~f:efun ~init:e
let pefun pe = lift2 efun (token "fun" *> pattern) (lift2 efunf (many pattern <* arr) pe)

let pelet pe =
  lift3
    elet
    (token "let" *> option Nonrec (token "rec" *> return Rec))
    (both pattern (lift2 efunf (many pattern <* token "=") pe))
    (token "in" *> pe)
;;

let pmul = token "*" *> return (ebinop Mul)
let pdiv = token "/" *> return (ebinop Div)
let padd = token "+" *> return (ebinop Add)
let psub = token "-" *> return (ebinop Sub)
let peq = token "=" *> return (ebinop Eq)
let plt = token "<" *> return (ebinop Lt)
let plte = token "<=" *> return (ebinop Lte)
let pneq = token "<>" *> return (ebinop Neq)
let pgt = token ">" *> return (ebinop Gt)
let pgte = token ">=" *> return (ebinop Gte)
let pand = token "&&" *> return (ebinop And)
let por = token "||" *> return (ebinop Or)

let expr =
  fix (fun expr ->
    let term = choice [ pevar; peconst; pelist expr; parens expr ] in
    let apply = chainl1 term (return eapply) in
    let poly = pepoly apply <|> apply in
    let fact = chainl1 poly (pmul <|> pdiv) in
    let sum = chainl1 fact (padd <|> psub) in
    let cons = chainr1 sum (dcol *> return econs) in
    let cmp1 = chainl1 cons (choice [ plte; pneq; plt; peq ]) in
    let cmp2 = chainl1 cmp1 (pgte <|> pgt) in
    let band = chainr1 cmp2 pand in
    let bor = chainr1 band por in
    let tuple = petuple bor <|> bor in
    let ife = peif tuple <|> tuple in
    choice [ pelet expr; pematch expr; pefun expr; ife ])
;;

(*----------------------------- Structure ------------------------------------*)

let str_item =
  let pseval = expr >>| fun e -> SEval e in
  let svalue f b = SValue (f, b) in
  let psvalue =
    lift2
      svalue
      (token "let" *> option Nonrec (token "rec" *> return Rec))
      (both pattern (lift2 efunf (many pattern <* token "=") expr))
  in
  choice [ pseval; psvalue ]
;;

let structure : structure t = sep_by dsemi str_item
let parse s = parse_string ~consume:Prefix structure s

let test_parse input =
  let open Stdlib.Format in
  match parse input with
  | Ok ast -> printf "%a\n" pp_structure ast
  | Error s -> printf "Parsing error: %s\n" s
;;
