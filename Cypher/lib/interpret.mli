(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

module NameMap : sig
  include Map.S with type key = string
end

module NameSet : sig
  include Set.S with type elt = string
end

type entity = int * NameSet.t * Ast.constant NameMap.t

module Node : sig
  type t = entity

  val compare : int * 'a * 'b -> int * 'c * 'd -> int
  val hash : 'a * 'b * 'c -> 'a
  val equal : int * 'a * 'b -> int * 'c * 'd -> bool
end

module Edge : sig
  type t = entity

  val compare : int * 'a * 'b -> int * 'c * 'd -> int
  val equal : int * 'a * 'b -> int * 'c * 'd -> bool
  val default : int * NameSet.t * 'a NameMap.t
end

(** graph type *)
type graph = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge).t

(** type of value that can be in output *)
type outvalue =
  | OutConstant of Ast.constant
  | OutNode of int * string list * (string * Ast.constant) list
  | OutRel of int * (int * string list * (string * Ast.constant) list) * int

val pp_outvalue : Format.formatter -> outvalue -> unit
val show_outvalue : outvalue -> string

(** type of output: ((num_creates_nodes, num_created_rels), (num_deleted_nodes, num_deleted_rels), RETURN_output) *)
type output = (int * int) * (int * int) * (string * outvalue) list list

val pp_output : Format.formatter -> output -> unit
val show_output : output -> string

(** type of error for evaluating expressions *)
type expr_error =
  | Type_mismatch
  | Division_by_zero
  | Undefined_id of string
  | Deleted_entity of int

val pp_expr_error : Format.formatter -> expr_error -> unit
val show_expr_error : expr_error -> string

(** type of error for interpreting request *)
type interpreter_error =
  | Type_mismatch
  | Multiple_def of string
  | Undefined_id of string
  | Deleted_entity of int
  | Expr_err of expr_error
  | Unable_node_deletion of int
  | ID_overflow
  | Undirected_rel
  | Multiple_using_rel_var of string

val pp_interpreter_error : Format.formatter -> interpreter_error -> unit
val show_interpreter_error : interpreter_error -> string

(** type of error for parsing and interpreting request *)
type req_error =
  | Parser_err of string
  | Interpreter_err of interpreter_error

val pp_req_error : Format.formatter -> req_error -> unit
val show_req_error : req_error -> string

(** creates empty graph *)
val create_empty_graph : graph

(** takes a graph and a request, interprets it and returns a new graph and an output or an error *)
val interpret_request : graph -> Ast.clause -> (graph * output, req_error) result

(** takes a graph and a request, parses and interprets it and returns a new graph and an output or an error *)
val parse_and_interpret_request : graph -> string -> (graph * output, req_error) result
