(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

module NameMap : sig
  type key = string
  type 'a t = 'a Map.Make(Base.String).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module NameSet : sig
  type elt = string
  type t = Set.Make(Base.String).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
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

(** graph module *)
module G : sig
  type t = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge).t

  module V : sig
    type t = entity

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    type label = t

    val create : label -> t
    val label : t -> label
  end

  type vertex = entity

  module E : sig
    type t = vertex * vertex * vertex

    val compare : t -> t -> int

    type vertex = entity

    val src : t -> vertex
    val dst : t -> vertex

    type label = vertex

    val create : vertex -> label -> vertex -> t
    val label : t -> label
  end

  type edge = E.t

  val is_directed : bool
  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int
  val out_degree : t -> vertex -> int
  val in_degree : t -> vertex -> int
  val mem_vertex : t -> vertex -> bool
  val mem_edge : t -> vertex -> vertex -> bool
  val mem_edge_e : t -> edge -> bool
  val find_edge : t -> vertex -> vertex -> edge
  val find_all_edges : t -> vertex -> vertex -> edge list
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list
  val iter_vertex : (vertex -> unit) -> t -> unit
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges : (vertex -> vertex -> unit) -> t -> unit
  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (edge -> unit) -> t -> unit
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  val map_vertex : (vertex -> vertex) -> t -> t
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
  val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val empty : t
  val add_vertex : t -> vertex -> t
  val remove_vertex : t -> vertex -> t
  val add_edge : t -> vertex -> vertex -> t
  val add_edge_e : t -> edge -> t
  val remove_edge : t -> vertex -> vertex -> t
  val remove_edge_e : t -> edge -> t
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
val create_empty_graph : G.t

(** takes a graph and a request, interprets it and returns a new graph and an output or an error *)
val interpret_request : graph -> Ast.clause -> (graph * output, req_error) result

(** takes a graph and a request, parses and interprets it and returns a new graph and an output or an error *)
val parse_and_interpret_request : graph -> string -> (graph * output, req_error) result
