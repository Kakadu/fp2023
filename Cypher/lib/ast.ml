(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

type name = string [@@deriving show { with_path = false }]

type constant =
  | Float of float
  | Int64 of int64
  | String of string
  | Bool of bool
  | Null
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

type un_op =
  | Minus
  | IS_NULL
  | IS_NOT_NULL
  | NOT
[@@deriving show { with_path = false }]

type list_op =
  | Eq
  | NEq
  | Less
  | Greater
  | LEq
  | GEq
[@@deriving show { with_path = false }]

type expression =
  | Const of constant
  | Var of name
  | Property of name * name
  | Bin_op of bin_op * expression * expression
  | Un_op of un_op * expression
  | List_op of expression * (list_op * expression) list
[@@deriving show { with_path = false }]

type pattern = name list * (name * expression) list
[@@deriving show { with_path = false }]

type direction =
  | Left
  | Right
  | No
[@@deriving show { with_path = false }] [@@deriving show { with_path = false }]

type entity_pattern = name option * pattern [@@deriving show { with_path = false }]

type path =
  { start_node_pt : entity_pattern
  ; rel_node_pts : ((entity_pattern * direction) * entity_pattern) list
  }
[@@deriving show { with_path = false }]

type where = expression list [@@deriving show { with_path = false }]

type order_by_cond =
  | Asc
  | Desc
[@@deriving show { with_path = false }]

type order_by = (expression list * order_by_cond) list
[@@deriving show { with_path = false }]

type star = All [@@deriving show { with_path = false }]

type delete_attr =
  | Detach
  | Nodetach
[@@deriving show { with_path = false }]

type clause =
  | With of star option * (expression * name) list * order_by * where * clause
  | Match of path list * where * clause
  | Create of path list * clause option
  | Delete of delete_attr * name list * clause option
  | Return of star option * (expression * name option) list * order_by
[@@deriving show { with_path = false }]
