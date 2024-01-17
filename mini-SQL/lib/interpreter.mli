(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

(** extended version of column type (meta information + index) *)
type int_column =
  { column_index : int
  ; meta : Types.column
  }

(** Expression type for executing *)
type expr =
  | Const of Types.item
  | Col of int_column
  | Plus of expr * expr
  | Minus of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NEqual of expr * expr
  | GThan of expr * expr
  | LThan of expr * expr
  | GThanEq of expr * expr
  | LThanEq of expr * expr

type join_type =
  | Inner of expr
  | Left of expr
  | Right of expr
  | Full of expr

(** SQL Query Operator Tree ( Load -> Join -> Restrict (WHERE) -> Project (SELECT) ) *)
type qot_node =
  | Load of Types.Table.t
  | Join of qot_node * qot_node * join_type
  | Restrict of qot_node * expr
  | Project of qot_node * expr list

module Eval : functor (M : Utils.MONAD_FAIL) -> sig
    (** Runs evaluation of AST.request on a specific database *)
  val eval : string -> Ast.request -> (Types.Table.t, Utils.error) M.t
end

val pp_int_column : Format.formatter -> int_column -> unit
val show_int_column : int_column -> string
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_join_type : Format.formatter -> join_type -> unit
val show_join_type : join_type -> string
val pp_qot_node : Format.formatter -> qot_node -> unit
val show_qot_node : qot_node -> string
