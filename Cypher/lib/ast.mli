(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

type name = string

val pp_name : Format.formatter -> name -> unit
val show_name : name -> string

type constant =
  | Float of float
  | Int64 of int64
  | String of name
  | Bool of bool
  | Null

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string

type bin_op =
  | CONTAINS
  | STARTS_WITH
  | ENDS_WITH
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Percent
  | Caret
  | AND
  | OR
  | XOR

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

type un_op =
  | Minus
  | IS_NULL
  | IS_NOT_NULL
  | NOT

val pp_un_op : Format.formatter -> un_op -> unit
val show_un_op : un_op -> string

type list_op =
  | Eq
  | NEq
  | Less
  | Greater
  | LEq
  | GEq

val pp_list_op : Format.formatter -> list_op -> unit
val show_list_op : list_op -> string

type expression =
  | Const of constant
  | Var of name
  | Property of name * name
  | Bin_op of bin_op * expression * expression
  | Un_op of un_op * expression
  | List_op of expression * (list_op * expression) list

val pp_expression : Format.formatter -> expression -> unit
val show_expression : expression -> string

type pattern = name list * (name * expression) list

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type direction =
  | Left
  | Right
  | No

val pp_direction : Format.formatter -> direction -> unit
val show_direction : direction -> string

type path =
  (name option * pattern)
  * ((name option * pattern * direction) * (name option * pattern)) list

val pp_path : Format.formatter -> path -> unit
val show_path : path -> string

type where = expression list

val pp_where : Format.formatter -> where -> unit
val show_where : where -> string

type order_by_cond =
  | Asc
  | Desc

val pp_order_by_cond : Format.formatter -> order_by_cond -> unit
val show_order_by_cond : order_by_cond -> string

type order_by = (expression list * order_by_cond) list

val pp_order_by : Format.formatter -> order_by -> unit
val show_order_by : order_by -> string

type star = All

val pp_star : Format.formatter -> star -> unit
val show_star : star -> string

type delete_attr =
  | Detach
  | Nodetach

val pp_delete_attr : Format.formatter -> delete_attr -> unit
val show_delete_attr : delete_attr -> string

type clause =
  | With of star option * (expression * name) list * order_by * where * clause
  | Match of path list * where * clause
  | Create of path list * clause option
  | Delete of delete_attr * name list * clause option
  | Return of star option * (expression * name option) list * order_by

val pp_clause : Format.formatter -> clause -> unit
val show_clause : clause -> string
