(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Env : sig
  type key = string
  type 'a t = 'a Map.Make(String).t

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

type value =
  | VNum of int
  | VAtom of string
  | VList of value list
  | VClosure of string list * Ast.term * value Env.t

module MonadFail : sig
  type 'a t =
    | Success of 'a
    | Failure of string

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t
end

module Interpreter : sig
  val ( let* ) : 'a MonadFail.t -> ('a -> 'b MonadFail.t) -> 'b MonadFail.t
  val value_to_term : value -> Ast.term
  val term_to_value : Ast.term -> value
  val unify : value Env.t -> Ast.term -> Ast.term -> value Env.t MonadFail.t

  val unify_lists
    :  value Env.t
    -> Ast.term list
    -> Ast.term list
    -> value Env.t MonadFail.t

  val eval_term : value Env.t -> Ast.term -> value MonadFail.t
  val eval_op : string -> value -> value -> value MonadFail.t
  val mapM : (Ast.term -> value MonadFail.t) -> Ast.term list -> value list MonadFail.t
  val eval : value Env.t -> Ast.term list -> value Env.t MonadFail.t
  val print_value : value -> string
  val solve : value Env.t -> Ast.term -> value Env.t MonadFail.t
  val interpret : Ast.many_term -> value Env.t option
  val print_env : value Env.t -> unit
end

val run : string -> unit
