(** Copyright 2021-2023, Kakadu, Ilya Syresenkov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* let rec fac x = if x = 1 then x else x * (fac x) *)

(*
   Todo:
   1. Implement signed int parse
   2. Think about type annotations support
   3. Implement match parsing
   4. Think about supporting list a :: b syntax
   5. Add tuples and lists parsers
   6. Add comments parser
*)

open Angstrom
open Base
open Ast

let pp printer parser str =
  Stdlib.Format.printf "%a" printer
  @@ Result.ok_or_failwith
  @@ parse_string ~consume:Consume.All parser str
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

let pint = ptoken @@ take_while1 is_digit >>| fun x -> EConst (CInt (int_of_string x))

let pbool =
  ptoken
  @@ choice
       [ pstoken "true" *> return true
       ; pstoken "false" *> return false
       ; fail "Failed to parse boolean"
       ]
  >>| fun x -> EConst (CBool x)
;;

let pconst = choice [ pint; pbool ]
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
       (pstoken "in" *> pexpr <|> return EUnit)
;;

let pbranch pexpr =
  ptoken
  @@ lift3
       (fun cond t f -> EBranch (cond, t, f))
       (pstoken "if" *> pexpr)
       (pstoken "then" *> pexpr)
       (pstoken "else" *> pexpr <|> return EUnit)
;;

let pebinop chain1 e pbinop = chain1 e (pbinop >>| fun op e1 e2 -> EBinop (op, e1, e2))
let plbinop = pebinop chainl1
let padd = pstoken "+" *> return Add
let psub = pstoken "-" *> return Sub
let pmul = pstoken "*" *> return Mul
let pdiv = pstoken "/" *> return Div
let peq = pstoken "=" *> return Eq
let pneq = pstoken "<>" *> return Neq

let pexpr =
  fix
  @@ fun pexpr ->
  let pe = choice [ pparens pexpr; pconst; pvar ] in
  let pe =
    lift2
      (fun f args -> List.fold_left ~f:(fun f arg -> EApp (f, arg)) ~init:f args)
      pe
      (many (char ' ' *> ptoken pe))
  in
  let pe = plbinop pe (pmul <|> pdiv) in
  let pe = plbinop pe (padd <|> psub) in
  let pe = plbinop pe (peq <|> pneq) in
  choice [ plet pexpr; pbranch pexpr; pe ]
;;

let parse = parse_string ~consume:Consume.All (pexpr <* pspaces)
