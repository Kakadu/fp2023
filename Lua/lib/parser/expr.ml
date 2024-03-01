(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom
open Ast
open Common

(** Parse constants *)

let parse_true = string "true" *> return Exp_true
let parse_false = string "false" *> return Exp_false
let parse_nil = string "nil" *> return Exp_nil

let parse_string =
  string "\"" *> many_till any_char (string "\"")
  >>= fun str -> return (Exp_string (Base.String.of_char_list str))
;;

let parse_number =
  take_while (function
    | '0' .. '9' | '.' | '-' -> true
    | _ -> false)
  >>= fun s ->
  try return (Exp_number (Float.of_string s)) with
  | Invalid_argument _ -> fail "not a number"
;;

(** Parse identsm function declaration, function call *)

let parse_explhs = parse_ident >>| fun lhs -> Exp_lhs lhs

let parse_function parse_block =
  let parse_idents =
    string "(" *> sep_by (ws *> string "," *> ws) parse_ident <* string ")"
  in
  lift2
    (fun idents bl -> Exp_function (idents, bl))
    (ws *> string "function" *> ws *> parse_idents)
    (parse_block <* string "end")
;;

let parse_apply pexpr =
  lift2
    (fun e1 e2 -> Call (e1, e2))
    (ws *> parse_explhs <* ws <* string "(")
    (sep_by (string ",") (ws *> pexpr) <* string ")")
;;

(** Parse function call in expression, example: n * fact(n-1) *)
let parse_expcall pexpr = parse_apply pexpr >>| fun e1 -> Exp_call e1

let parse_single_expr pblock pexpr =
  ws
  *> choice
       [ parse_function pblock
       ; parse_expcall pexpr
       ; parse_false
       ; parse_true
       ; parse_nil
       ; parse_number
       ; parse_string
       ; parse_explhs
       ]
;;

(** Parse arithmetic *)

let pmul = string "*" *> return (fun x y -> Exp_op (Op_mul, x, y))
let pdiv = string "/" *> return (fun x y -> Exp_op (Op_div, x, y))
let padd = string "+" *> return (fun x y -> Exp_op (Op_add, x, y))
let psub = string "-" *> return (fun x y -> Exp_op (Op_sub, x, y))
let pmod = string "%" *> return (fun x y -> Exp_op (Op_mod, x, y))
let pcon = string ".." *> return (fun x y -> Exp_op (Op_concat, x, y))
let peq = string "==" *> return (fun x y -> Exp_op (Op_eq, x, y))
let pneq = string "~=" *> return (fun x y -> Exp_op (Op_neq, x, y))
let plt = string "<" *> return (fun x y -> Exp_op (Op_lt, x, y))
let pgt = string ">" *> return (fun x y -> Exp_op (Op_lt, y, x))
let ple = string "<=" *> return (fun x y -> Exp_op (Op_le, x, y))
let pge = string ">=" *> return (fun x y -> Exp_op (Op_le, y, x))
let pand = string "and" *> return (fun x y -> Exp_op (Op_and, x, y))
let por = string "or" *> return (fun x y -> Exp_op (Op_or, x, y))

let parse_expr pblock =
  fix (fun pexpr ->
    let pep =
      chainl1 (parse_single_expr pblock pexpr) (ws *> (pcon <|> pmul <|> pdiv <|> pmod))
    in
    let pep = chainl1 pep (ws *> (padd <|> psub)) in
    let pep =
      chainl1 pep (ws *> (peq <|> ple <|> plt <|> pge <|> pgt <|> pand <|> por <|> pneq))
    in
    pep)
;;
