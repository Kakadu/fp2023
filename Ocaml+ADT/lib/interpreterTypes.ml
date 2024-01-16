(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
module StringMap = Map.Make (String)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VList of value list
  | VTuple of value list
  | VAdt of string * value option
  | VFun of pattern * decl_exp * env
  | VLetRec of string * value

and env = value StringMap.t [@@deriving eq]

let vint num = VInt num
let vstring str = VString str
let vbool bool = VBool bool
let vlist lst = VList lst
let vtuple tuple = VTuple tuple
let vadt type_name type_value = VAdt (type_name, type_value)
let vfun pattern decl_exp env = VFun (pattern, decl_exp, env)
let vletrec let_name let_value = VLetRec (let_name, let_value)

let rec pp_value_list fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" pp_value h
  | h :: tl -> fprintf fmt "%a; %a" pp_value h pp_value_list tl

and pp_value_tuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" pp_value h
  | h :: tl -> fprintf fmt "%a, %a" pp_value h pp_value_tuple tl

and pp_value fmt = function
  | VInt num -> fprintf fmt "%d" num
  | VString str -> fprintf fmt "%S" str
  | VBool bool -> fprintf fmt "%b" bool
  | VList lst -> fprintf fmt "[%a]" pp_value_list lst
  | VTuple tuple -> fprintf fmt "(%a)" pp_value_tuple tuple
  | VAdt (type_name, Some type_value) -> fprintf fmt "%s %a" type_name pp_value type_value
  | VAdt (type_name, None) -> fprintf fmt "%s" type_name
  | VFun (_, _, _) -> fprintf fmt "<fun>"
  | VLetRec (_, _) -> fprintf fmt "<rec fun>"
;;

let pp_env fmt (environment : env) =
  StringMap.iter (fun key data -> fprintf fmt "%S: %a\n" key pp_value data) environment
;;

type failure =
  | UnboundVariable of string
  | ValueTypeError of value
  | ExprTypeError of string
  | DivisionByZeroError
  | ExecError of value * value
  | PatternMatchingError

let pp_failure fmt = function
  | UnboundVariable str -> fprintf fmt "UnboundVariable: %S" str
  | ValueTypeError err_val -> fprintf fmt "ValueTypeError: %a" pp_value err_val
  | ExprTypeError err_expr -> fprintf fmt "TypeError: %S" err_expr
  | DivisionByZeroError -> fprintf fmt "DivisionByZeroError"
  | ExecError (val1, val2) -> fprintf fmt "ExecError: %a # %a" pp_value val1 pp_value val2
  | PatternMatchingError -> fprintf fmt "PatternMatchingError"
;;
