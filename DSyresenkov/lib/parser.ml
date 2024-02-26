(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* let rec fac x = if x = 1 then x else x * (fac x) *)

(*
   Type anotations
   let f : int -> int -> bool = fun x y -> x = y
   let f (x : int) (y : int) : bool = x = y
*)

open Angstrom
open Base
open Ast

let pp printer parser input =
  match parse_string ~consume:Consume.All parser input with
  | Result.Ok res -> Stdlib.Format.printf "%a" printer res
  | _ -> Stdlib.print_endline "Failed to parse"
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter x = is_lowercase x || is_uppercase x

(* Not all keywords are forbidden *)
let is_keyword = function
  | "let"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "fun"
  | "type"
  | "int"
  | "string"
  | "bool" -> true
  | _ -> false
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let pspaces = skip_while is_space
let ptoken p = pspaces *> p
let pstoken s = pspaces *> Angstrom.string s
let pparens p = pstoken "(" *> p <* pstoken ")"

let pid =
  let pfirst =
    satisfy (fun ch -> is_letter ch || Char.equal ch '_') >>| fun ch -> Char.escaped ch
  in
  let plast = take_while (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_') in
  ptoken @@ lift2 (fun x y -> x ^ y) pfirst plast
  >>= fun s -> if is_keyword s then fail "Keyword identifiers are forbidden" else return s
;;

let pint = ptoken @@ take_while1 is_digit >>| fun x -> CInt (int_of_string x)

let pbool =
  ptoken
  @@ choice
       [ pstoken "true" *> return true
       ; pstoken "false" *> return false
       ; fail "Failed to parse boolean"
       ]
  >>| fun x -> CBool x
;;

let punit = pstoken "()" *> return CUnit
let pconst = choice [ pint; pbool; punit ] >>| fun x -> EConst x
let pvar = pid >>| fun e -> EVar e

let plet pexpr =
  let rec pbody pexpr =
    pid >>= fun id -> pbody pexpr <|> pstoken "=" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> ELet (r, id, e1, e2))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (pstoken "()" <|> pid)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr >>| (fun x -> Some x) <|> return None)
;;

let pbranch pexpr =
  ptoken
  @@ lift3
       (fun cond t f -> EBranch (cond, t, f))
       (pstoken "if" *> pexpr)
       (pstoken "then" *> pexpr)
       (pstoken "else" *> pexpr <|> return (EConst CUnit))
;;

let plist pexpr =
  let psqparens p = pstoken "[" *> p <* pstoken "]" in
  psqparens @@ sep_by (pstoken ";") pexpr >>| fun x -> EList x
;;

let ppconst = choice [ pint; pbool ] >>| fun x -> PConst x
let ppvar = pid >>| fun x -> PVar x

let ppattern =
  fix
  @@ fun ppattern ->
  let ppt =
    choice
      [ pparens ppattern
      ; ppconst
      ; (pstoken "_" >>| fun _ -> PWild)
      ; (pstoken "[]" >>| fun _ -> PEmpty)
      ; ppvar
      ]
  in
  let ppt =
    lift2
      (fun p1 -> function
        | h :: tl -> PCons (p1, h, tl)
        | _ -> p1)
      ppt
      (many (pstoken "::" *> ppt))
  in
  let ppt =
    lift2
      (fun p -> function
        | h :: tl -> PTuple (p, h, tl)
        | _ -> p)
      ppt
      (many (pstoken "," *> ppt))
  in
  let ppt =
    lift2
      (fun p1 -> function
        | h :: tl -> POr (p1, h, tl)
        | _ -> p1)
      ppt
      (many (pstoken "|" *> ppt))
  in
  ppt
;;

let pmatch pexpr =
  let pcase ppattern pexpr =
    lift2 (fun p e -> p, e) (pstoken "|" *> ppattern) (pstoken "->" *> pexpr)
  in
  lift2
    (fun expr cases -> EMatch (expr, cases))
    (pstoken "match" *> pexpr <* pstoken "with")
    (many1 (pcase ppattern pexpr))
;;

let pfun pexpr =
  let rec pbody pexpr =
    pid >>= fun id -> pbody pexpr <|> pstoken "->" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "fun" *> pbody pexpr
;;

let pebinop chain1 e pbinop = chain1 e (pbinop >>| fun op e1 e2 -> EBinop (op, e1, e2))
let plbinop = pebinop chainl1
let padd = pstoken "+" *> return Add
let psub = pstoken "-" *> return Sub
let pmul = pstoken "*" *> return Mul
let pdiv = pstoken "/" *> return Div
let peq = pstoken "=" *> return Eq
let pneq = pstoken "<>" *> return Neq
let ples = pstoken "<" *> return Les
let pleq = pstoken "<=" *> return Leq
let pgre = pstoken ">" *> return Gre
let pgeq = pstoken ">=" *> return Geq

let pexpr =
  fix
  @@ fun pexpr ->
  let pe = choice [ pparens pexpr; pconst; pvar; plist pexpr; pfun pexpr ] in
  let pe =
    lift2
      (fun f args -> List.fold_left ~f:(fun f arg -> EApp (f, arg)) ~init:f args)
      pe
      (many (char ' ' *> ptoken pe))
  in
  let pe = plbinop pe (pmul <|> pdiv) in
  let pe = plbinop pe (padd <|> psub) in
  let pe = plbinop pe (choice [ peq; pneq; pgeq; pleq; ples; pgre ]) in
  let pe =
    lift2
      (fun e -> function
        | h :: tl -> ETuple (e, h, tl)
        | _ -> e)
      pe
      (many (pstoken "," *> pe))
  in
  choice [ plet pexpr; pbranch pexpr; pmatch pexpr; pfun pexpr; pe ]
;;

let parse_expr = parse_string ~consume:Consume.All (pexpr <* pspaces)
let parse = parse_string ~consume:Consume.All (many1 (plet pexpr) <* pspaces)
