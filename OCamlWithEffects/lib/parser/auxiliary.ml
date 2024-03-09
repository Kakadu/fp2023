(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

(* Determinant of the type of a steamed expression *)

type ast_type =
  | DeclarationList (* [SDeclaration (...) ; SDeclaration (...) ; SDeclaration (...)] *)
  | MixedList (* [SDeclaration (...) ; SExpression (...)]*)
  | FreeExpression (* SExpression (...) *)

(* This function, given an ast, determines whether ast is a program (a list of declarations),
   a free expression, or a list containing arbitrary expressions. *)
let determine_ast_type ast =
  match ast with
  | [ SExpression _ ] -> FreeExpression
  | _ ->
    let rec helper = function
      | [] -> DeclarationList
      | hd :: tl ->
        (match hd with
         | SDeclaration _ -> helper tl
         | _ -> MixedList)
    in
    helper ast
;;

(* ---------------- *)

(* Helper functions for parsing *)

let is_keyword = function
  | "continue"
  | "effect"
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "match"
  | "perform"
  | "rec"
  | "then"
  | "true"
  | "try"
  | "with" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_ident c = is_lower c || is_upper c || c = '_'

let is_acceptable_fl = function
  | Some c when is_lower c || c = '_' -> return c
  | _ -> fail "abc"
;;

let rec chainr1 e op =
  e >>= fun se -> op >>= (fun f -> chainr1 e op >>| f se) <|> return se
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* ---------------- *)

(* Constructors for expressions *)

let econst x = EConst x
let ebinop op left_op right_op = EBinaryOperation (op, left_op, right_op)
let eunop operator operand = EUnaryOperation (operator, operand)
let elist cont = EList cont
let elistcons l r = EListCons (l, r)
let etuple cont = ETuple cont
let eidentifier x = EIdentifier x
let eapplication f x = EApplication (f, x)
let efun var expression = EFun (var, expression)
let elet_in func_name expression exp_in = ELetIn (func_name, expression, exp_in)
let erec_let_in func_name expression exp_in = ERecLetIn (func_name, expression, exp_in)
let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)
let etry_with expression handlers = ETryWith (expression, handlers)
let eeffect_without_arguments name = EEffectWithoutArguments name
let eefect_with_arguments name arg = EEffectWithArguments (name, arg)
let eeffect_perform expr = EEffectPerform expr
let eeffect_continue cont expr = EEffectContinue (cont, expr)
let econt_val n = Continue n

(* ---------------- *)

(* Constructors for declarations *)

let ddeclaration func_name expression = DDeclaration (func_name, expression)
let drec_declaration func_name expression = DRecDeclaration (func_name, expression)
let deffect_declaration name typ = DEffectDeclaration (name, typ)

(* ---------------- *)

(* Constructors for structure items *)

let sdeclaration decl = SDeclaration decl
let sexpression expr = SExpression expr

(* ---------------- *)

(* Constructors for binary operations *)

let sadd _ = Add
let ssub _ = Sub
let smul _ = Mul
let sdiv _ = Div
let seq _ = Eq
let sneq _ = NEq
let sgt _ = Gt
let sgte _ = Gte
let slt _ = Lt
let slte _ = Lte
let sand _ = And
let sor _ = Or

(* ---------------- *)

(* Constructors for unary operations *)

let umin _ = Minus
let unot _ = Not
let uplus _ = Plus

(* ---------------- *)

(* Constructors for patterns *)

let pany _ = PAny
let pnill _ = PNill
let pconst c = PConst c
let pval v = PVal v
let plist_cons l r = PListCons (l, r)
let ptuple l = PTuple l
let peffect_without_args name = PEffectWithoutArguments name
let peffect_with_args name a = PEffectWithArguments (name, a)

(* ---------------- *)

(* Constructors for annotations *)

let aint = AInt
let abool = ABool
let achar = AChar
let astring = AString
let aunit = AUnit
let aarrow l r = AArrow (l, r)
let alist a = AList a
let atuple alist = ATuple alist
let aeffect a = AEffect a

(* ----------------- *)

(* Constructors for effect handlers *)

let effecthandler pat expr cont = EffectHandler (pat, expr, cont)
let continue k = Continue k

(* ----------------- *)
