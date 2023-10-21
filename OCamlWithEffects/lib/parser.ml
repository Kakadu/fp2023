(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* Constructors for expressions *)
let identifier x = Identifier x

let declraration func_name var_list expression =
  Declaration (func_name, var_list, expression)
;;

let rec_declraration func_name var_list expression =
  Declaration (func_name, var_list, expression)
;;

(* ---------------- *)

let keywords = [ "let"; "rec"; "match"; "with"; "if"; "then"; "else"; "in"; "fun"; "and" ]
