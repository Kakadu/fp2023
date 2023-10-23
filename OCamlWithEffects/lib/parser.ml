(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

(* Constructors for expressions *)
let econst x = EConst x
let ebinop op left_op right_op = EBinaryOperation (op, left_op, right_op)
let eunop operator operand = EUnaryOperation (operator, operand)
let identifier x = EIdentifier x
let eapplication f x = EApplication (f, x)
let efun var_list expression = EFun (var_list, expression)

let declraration func_name var_list expression =
  EDeclaration (func_name, var_list, expression)
;;

let rec_declraration func_name var_list expression =
  EDeclaration (func_name, var_list, expression)
;;

let eif_then_else condition true_b false_b = EIfThenElse (condition, true_b, false_b)
let ematch_with expression cases = EMatchWith (expression, cases)
(* ---------------- *)

let keywords = [ "let"; "rec"; "match"; "with"; "if"; "then"; "else"; "in"; "fun"; "and" ]

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_wspace = skip_while is_whitespace

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
