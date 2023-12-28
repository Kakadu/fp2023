(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | Int of int
  | String of string
  | List of value list
  | Bool of bool
  | None

val equal_value : value -> value -> bool
val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

type arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

val equal_arith_op : arith_op -> arith_op -> bool
val pp_arith_op : Format.formatter -> arith_op -> unit
val show_arith_op : arith_op -> string

type identifier = Identifier of string

val equal_identifier : identifier -> identifier -> bool
val pp_identifier : Format.formatter -> identifier -> unit
val show_identifier : identifier -> string

type modifier =
  | Global
  | Class

val equal_modifier : modifier -> modifier -> bool
val pp_modifier : Format.formatter -> modifier -> unit
val show_modifier : modifier -> string

type bool_op =
  | And
  | Or
  | Equal
  | NotEqual
  | GreaterOrEqual
  | Greater
  | LessOrEqual
  | Less

val equal_bool_op : bool_op -> bool_op -> bool
val pp_bool_op : Format.formatter -> bool_op -> unit
val show_bool_op : bool_op -> string

type f_string_type =
  | Str of value
  | Var of identifier

val equal_f_string_type : f_string_type -> f_string_type -> bool
val pp_f_string_type : Format.formatter -> f_string_type -> unit
val show_f_string_type : f_string_type -> string

type f_string_elem = FStringElem of f_string_type

and expression =
  | Const of value
  | Variable of modifier * identifier
  | ArithOp of arith_op * expression * expression
  | BoolOp of bool_op * expression * expression
  | FunctionCall of identifier * expression list
  | List of expression list
  | Field of identifier * identifier
  | MethodCall of identifier * identifier * expression list
  | Lambda of identifier list * expression
  | Object of identifier * expression list
  | FString of f_string_elem list

val equal_f_string_elem : f_string_elem -> f_string_elem -> bool
val equal_expression : expression -> expression -> bool
val pp_f_string_elem : Format.formatter -> f_string_elem -> unit
val show_f_string_elem : f_string_elem -> string
val pp_expression : Format.formatter -> expression -> unit
val show_expression : expression -> string

type statement =
  | Expression of expression
  | Assign of expression * expression
  | Function of identifier * identifier list * statement list
  | IfElse of expression * statement list * statement list
  | Else of statement list
  | While of expression * statement list
  | For of expression * expression list * statement list
  | Class of identifier * statement list
  | Return of expression

val equal_statement : statement -> statement -> bool
val pp_statement : Format.formatter -> statement -> unit
val show_statement : statement -> string

type flag =
  | No
  | Return_f
