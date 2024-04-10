(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

type name = string

val pp_name : Format.formatter -> name -> unit
val show_name : name -> string

type constant =
  | Float of float (** 4.4 *)
  | Int64 of int64 (** 4 *)
  | String of string (** "Hola" *)
  | Bool of bool (** TRUE *)
  | Null (** NULL *)

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string

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

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

(** Unary operators, e. g. n.id IS NULL *)
type un_op =
  | Minus (** - *)
  | IS_NULL (** IS NULL *)
  | IS_NOT_NULL (** IS NOT NULL *)
  | NOT (** NOT *)

val pp_un_op : Format.formatter -> un_op -> unit
val show_un_op : un_op -> string

(** Comparison operators, e. g. 5 <= 6 >= 3 = n.id *)
type list_op =
  | Eq (** = *)
  | NEq (** <> *)
  | Less (** < *)
  | Greater (** > *)
  | LEq (** <= *)
  | GEq (** >= *)

val pp_list_op : Format.formatter -> list_op -> unit
val show_list_op : list_op -> string

(** Arithmetic expressions *)
type expression =
  | Const of constant (** 5, 4.4, "Hola" *)
  | Var of name (** n, `hola` *)
  | Property of name * name (** n.id *)
  | Bin_op of bin_op * expression * expression (** 5 + 6 *)
  | Un_op of un_op * expression (** NEG TRUE, -(5), a.name IS NULL *)
  | List_op of expression * (list_op * expression) list (** 5 <= 6.6 > 4 = 4.0 *)

val pp_expression : Format.formatter -> expression -> unit
val show_expression : expression -> string

(** Patterns of node or relationship, e. g. :Person:Director\{name:"John"\}*)
type pattern = name list * (name * expression) list

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

(** Relationship direction *)
type direction =
  | Left (** <-\[...\]- *)
  | Right (** --> *)
  | No (** -- *)

val pp_direction : Format.formatter -> direction -> unit
val show_direction : direction -> string

(** Entity pattern: (node:Label1:Label2{some:"some"}), \[rel:Label1:Label2{some:"some"}\] *)
type entity_pattern = name option * pattern

val pp_entity_pattern : Format.formatter -> entity_pattern -> unit
val show_entity_pattern : entity_pattern -> string

(** Path pattern [(n:Person)]  [-->()]  [<-[:Loves]-(\{name:"John"\})] *)
type path =
  { start_node_pt : entity_pattern
  ; rel_node_pts : ((entity_pattern * direction) * entity_pattern) list
  }

val pp_path : Format.formatter -> path -> unit
val show_path : path -> string

(** WHERE subclause, e. g. WHERE n.id = 5 *)
type where = expression list

val pp_where : Format.formatter -> where -> unit
val show_where : where -> string

(** ORDER BY subclause condition*)
type order_by_cond =
  | Asc (** ORDER BY n.id ASC, ORDER BY n.id *)
  | Desc (** ORDER BY n.id DESC *)

val pp_order_by_cond : Format.formatter -> order_by_cond -> unit
val show_order_by_cond : order_by_cond -> string

(** ORDER BY subclause, e. g. ORDER BY n.id ASC, ORDER BY n.age, n.scores DESC *)
type order_by = (expression list * order_by_cond) list

val pp_order_by : Format.formatter -> order_by -> unit
val show_order_by : order_by -> string

(** Projection of all aliases *)
type star = All (** RETURN *, WITH * *)

val pp_star : Format.formatter -> star -> unit
val show_star : star -> string

(** DELETE attribute *)
type delete_attr =
  | Detach (** DETACH DELETE *)
  | Nodetach (** NODETACH DELETE *)

val pp_delete_attr : Format.formatter -> delete_attr -> unit
val show_delete_attr : delete_attr -> string

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

val pp_clause : Format.formatter -> clause -> unit
val show_clause : clause -> string
