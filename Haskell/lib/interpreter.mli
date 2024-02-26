(** Copyright 2023-2024, Danil P*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module LazyResult : sig
  type ('a, 'e) t =
    | Result of ('a, 'e) result
    | Thunk of (unit -> ('a, 'e) t)

  val return : 'a -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  val fail : 'e -> ('a, 'e) t
  val force : ('a, 'e) t -> ('a, 'e) t
  val thunk : (unit -> ('a, 'b) t) -> ('a, 'b) t
end

module EnvTypes : sig
  type err =
    | NotInScopeError of string
    | DivisionByZeroError
    | NonExhaustivePatterns of string
    | TypeMismatch

  type res = (value, err) LazyResult.t
  and environment = (string, res, Base.String.comparator_witness) Base.Map.t

  and value =
    | ValInt of int
    | ValBool of bool
    | ValString of string
    | ValChar of char
    | ValNil
    | ValCons of res * res
    | ValTuple of res list
    | ValFun of Ast.pat * Ast.expr * environment
    | ValEmptyTree
    | ValTree of res * res * res
end

module Env : sig
  include module type of EnvTypes

  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b option
  val update : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val pp_err : Format.formatter -> err -> unit
  val pp_value : Format.formatter -> value -> unit
  val pp_environment : Format.formatter -> environment -> unit
  val pp_value_t : Format.formatter -> (value, err) LazyResult.t -> unit
end

module Eval : sig
  val interpret : Ast.prog -> unit
  val eval_prog : Ast.prog -> (Env.environment, Env.err) LazyResult.t
end
