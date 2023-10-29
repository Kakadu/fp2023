(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let digit_c =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy is_digit
;;

let l_low = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let l_up = function
  | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let variable = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let letter = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let skip_whitespace = take_while is_whitespace
let skip_whitespace1 = take_while1 is_whitespace
let token s = skip_whitespace *> string s
let ptoken p = spaces *> p
let lp = token "("
let rp = token ")"
let add = token "+" *> return Add
let sub = token "-" *> return Sub
let mul = token "*" *> return Mul
let div = token "/" *> return Div
let high_pr_op = skip_whitespace *> mul <|> div <* skip_whitespace
let low_pr_op = skip_whitespace *> add <|> sub <* skip_whitespace
let peq = token "=" *> return Eq
let pneq = token "<>" *> return Neq
let parens p = char '(' *> p <* char ')'

let is_syntax = function
  | "let"
  | "open"
  | "if"
  | "else"
  | "fun"
  | "function"
  | "then"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with"
  | "object"
  | "end" -> true
  | _ -> false
;;

let if_then_else elem =
  ptoken
  @@ lift3
       (fun iff thenn elsee -> IfThenElse (iff, thenn, elsee))
       (token "if" *> elem)
       (token "then" *> elem)
       (token "else" *> elem <|> return Empty)
;;

let sign =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Sign or digit expected"
;;

let dot =
  peek_char
  >>= function
  | Some '.' -> advance 1 >>| fun () -> true
  | _ -> return false
;;

let number =
  skip_whitespace *> sign
  >>= fun sign ->
  take_while1 is_digit
  >>= fun whole ->
  dot
  >>= function
  | false -> return (Const (Intenger (int_of_string (sign ^ whole))))
  | true ->
    take_while1 is_digit
    >>= fun part -> return (Const (Float (float_of_string (sign ^ whole ^ "." ^ part))))
;;

let bool_pars =
  ptoken
  @@ choice
       [ token "true" *> return true; token "false" *> return false; fail "Not boolean" ]
  >>| fun x -> Const (Bool x)
;;

let ident =
  skip_whitespace *> peek_char
  >>= function
  | Some x when l_low x ->
    take_while l_low
    >>= fun a -> if is_syntax a then fail "Reserved identifier" else return a
  | _ -> fail "Invalid first char"
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let let_pars pexpr =
  let rec fun_pars pexpr =
    ident >>= fun id -> fun_pars pexpr <|> token "=" *> pexpr >>| fun e -> Fun (id, e)
  in
  token "let"
  *> lift4
       (fun r id e1 e2 -> Let (r, id, e1, e2))
       (token "rec" *> return Rec <|> return NoRec)
       (token "()" <|> ident)
       (token "=" *> pexpr <|> fun_pars pexpr)
       (token "in" *> pexpr <|> return Empty)
;;

let pebinop chain1 e pbinop = chain1 e (pbinop >>| fun op l r -> Binop (op, l, r))
let plbinop = pebinop chainl1
let var_pars = ident >>= fun x -> return (Ast.Var x)
let const_pars = choice [ bool_pars; number ]

let pexpr =
  fix (fun parseExpression ->
    let atom = choice [ parens parseExpression; const_pars; var_pars ] in
    let apply =
      lift2
        (fun f args -> List.fold_left ~f:(fun f arg -> App (f, arg)) ~init:f args)
        atom
        (many (char ' ' *> ptoken atom))
    in
    let multiplyDivide = plbinop apply high_pr_op in
    let addSubtract = plbinop multiplyDivide low_pr_op in
    let equalNotEqual = plbinop addSubtract (peq <|> pneq) in
    let letParser = let_pars parseExpression in
    let ifParser = if_then_else parseExpression in
    choice [ letParser; ifParser; equalNotEqual ])
;;

let parser = parse_string ~consume:Consume.All (pexpr <* spaces)
