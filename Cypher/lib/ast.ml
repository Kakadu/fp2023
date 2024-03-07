(** Copyright 2023-2024, Arseniy Baytenov *)

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

(** Patterns of node or relationship, e. g. :Person:Director\{name:"John"\}*)
type pattern = name list * (name * expression) list
[@@deriving show { with_path = false }]

(** Relationship direction *)
type direction =
  | Left (** <-\[...\]- *)
  | Right (** --> *)
  | No (** -- *)
[@@deriving show { with_path = false }] [@@deriving show { with_path = false }]

(** Path pattern (n:Person)-->()<-\[:Loves\]-(\{name:"John"\}) *)
type path =
  (name option * pattern)
  * ((name option * pattern * direction) * (name option * pattern)) list
[@@deriving show { with_path = false }]

(** WHERE subclause, e. g. WHERE n.id = 5 *)
type where = expression list [@@deriving show { with_path = false }]

(** ORDER BY subclause condition*)
type order_by_cond =
  | Asc (** ORDER BY n.id ASC, ORDER BY n.id *)
  | Desc (** ORDER BY n.id DESC *)
[@@deriving show { with_path = false }]

(** ORDER BY subclause, e. g. ORDER BY n.id ASC, ORDER BY n.age, n.scores DESC *)
type order_by = (expression list * order_by_cond) list
[@@deriving show { with_path = false }]

(** Projection of all aliases, e. g. RETURN * *)
type star = All [@@deriving show { with_path = false }]

(** DELETE attribute *)
type delete_attr =
  | Detach (** DETACH DELETE *)
  | Nodetach (** NODETACH DELETE *)
[@@deriving show { with_path = false }]

(** Clauses *)
type clause =
  | With of star option * (expression * name) list * order_by * where * clause
  (** WITH \[ORDER BY\] \[WHERE\] *)
  | Match of path list * where * clause (** MATCH \[WHERE\] *)
  | Create of path list * clause option (** CREATE *)
  | Delete of delete_attr * name list * clause option
  (** DELETE, DETACH DELETE, NODETACH DELETE *)
  | Return of star option * (expression * name option) list * order_by
  (** RETURN \[ORDER BY\] *)
[@@deriving show { with_path = false }]
