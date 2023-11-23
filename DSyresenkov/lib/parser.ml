(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* let rec fac x = if x = 1 then x else x * (fac x) *)

(*
   Todo:
   1. Implement unary operators
   2. Think about type annotations support
   3. Think about supporting list a :: b syntax
   4. Add comments parser
*)

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

let pconst = choice [ pint; pbool ] >>| fun x -> EConst x
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
      (fun p1 ps ->
        match ps with
        | h :: tl -> PCons (p1, h, tl)
        | _ -> p1)
      ppt
      (many (pstoken "::" *> ppt))
  in
  let ppt =
    lift2
      (fun p1 ps ->
        match ps with
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
  let pe = choice [ pparens pexpr; pconst; pvar; plist pexpr ] in
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
      (fun e1 es ->
        match es with
        | h :: tl -> ETuple (e1, h, tl)
        | _ -> e1)
      pe
      (many (pstoken "," *> pe))
  in
  choice [ plet pexpr; pbranch pexpr; pmatch pexpr; pfun pexpr; pe ]
;;

let parse = parse_string ~consume:Consume.All (many1 (plet pexpr) <* pspaces)

(** Tests *)

let%expect_test _ =
  pp pp_expr (plet pexpr) "let f x = x";
  [%expect {| (ELet (NonRec, "f", (EFun ("x", (EVar "x"))), EUnit)) |}]
;;

let%expect_test _ =
  pp pp_expr (pbranch pexpr) "if f x then x else 0";
  [%expect
    {| (EBranch ((EApp ((EVar "f"), (EVar "x"))), (EVar "x"), (EConst (CInt 0)))) |}]
;;

let%expect_test _ =
  pp pp_expr (plist pexpr) "[1 + 2; 1 * 2; 1 - 2; 1 / 2]";
  [%expect
    {| 
    (EList
       [(EBinop (Add, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Mul, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Sub, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Div, (EConst (CInt 1)), (EConst (CInt 2))))]) 
    |}]
;;

let%expect_test _ =
  pp
    pp_expr
    (pmatch pexpr)
    {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
  [%expect
    {|
        (EMatch ((EVar "x"),
           [((PCons ((PVar "h1"), (PVar "h2"), [(PVar "tl")])),
             (EBranch ((EBinop (Geq, (EVar "h1"), (EVar "h2"))), (EVar "h1"),
                (EVar "h2"))));
             ((PCons ((PVar "h1"), PEmpty, [])), (EVar "h1"));
             (PWild, (EConst (CInt 0)))]
           )) 
    |}]
;;

let%expect_test _ =
  pp pp_expr pexpr "[1; 2], [3; 4]";
  [%expect
    {|
    (ETuple ((EList [(EConst (CInt 1)); (EConst (CInt 2))]),
       (EList [(EConst (CInt 3)); (EConst (CInt 4))]), [])) 
    |}]
;;

let%expect_test _ =
  pp pp_expr pexpr "[1, 2, 3; 4, 5, 6]";
  [%expect
    {|
    (EList
       [(ETuple ((EConst (CInt 1)), (EConst (CInt 2)), [(EConst (CInt 3))]));
         (ETuple ((EConst (CInt 4)), (EConst (CInt 5)), [(EConst (CInt 6))]))]) 
    |}]
;;
