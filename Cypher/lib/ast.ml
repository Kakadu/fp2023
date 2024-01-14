(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

type name = string [@@deriving show { with_path = false }]

type constant =
  | Float of float (** 4.4 *)
  | Int64 of int64 (** 4 *)
  | String of string (** "Hola" *)
  | Bool of bool (** TRUE *)
  | Null (** NULL *)
[@@deriving show { with_path = false }]

(** Binary operators, e. g. 5 + 10 *)
type bin_op =
  | CONTAINS (** CONTAINS *)
  | STARTS_WITH (** STARTS WITH *)
  | ENDS_WITH (** ENDS WITH *)
  | Plus (** + *)
  | Minus (** - *)
  | Slash (** / *)
  | Asterisk (** * *)
  | Percent (** % *)
  | Caret (** ^ *)
  | AND (** AND *)
  | OR (** OR *)
  | XOR (** XOR *)
[@@deriving show { with_path = false }]

(** Unary operators, e. g. n.id IS NULL *)
type un_op =
  | Minus (** - *)
  | IS_NULL (** IS NULL *)
  | IS_NOT_NULL (** IS NOT NULL *)
  | NOT (** NOT *)
[@@deriving show { with_path = false }]

(** Comparison operators, e. g. 5 <= 6 >= 3 = n.id *)
type list_op =
  | Eq (** = *)
  | NEq (** <> *)
  | Less (** < *)
  | Greater (** > *)
  | LEq (** <= *)
  | GEq (** >= *)
[@@deriving show { with_path = false }]

(** Arithmetic expressions *)
type expression =
  | Const of constant (** 5, 4.4, "Hola" *)
  | Var of name (** n, `hola` *)
  | Property of name * name (** n.id *)
  | Bin_op of bin_op * expression * expression
  | Un_op of un_op * expression
  | List_op of expression * (list_op * expression) list
[@@deriving show { with_path = false }]

(** Patterns of node or relationship, e. g. :Person:Director{name:"John"}*)
type pattern = name list * (name * expression) list
[@@deriving show { with_path = false }]

(** Relationship direction *)
type direction =
  | Left (** <-[...]- *)
  | Right (** --> *)
  | No (** -- *)
[@@deriving show { with_path = false }] [@@deriving show { with_path = false }]

(** Path pattern (n:Person)-->()<-[:Loves]-({name:"John"}) *)
type path =
  (name option * pattern)
  * ((name option * pattern * direction) * (name option * pattern)) list
[@@deriving show { with_path = false }]
