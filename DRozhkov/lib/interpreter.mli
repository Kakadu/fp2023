(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONAD_ERROR = sig
  type ('a, 'e) t

  val fail : 'e -> ('a, 'e) t
  val return : 'a -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VFun of string * Ast.expression * bundle

and bundle = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type error =
  | Division_by_zero
  | Pattern_matching_error
  | Unbound_value of string
  | Incorrect_type of value

val pp_error : Format.formatter -> error -> unit

module Interpret : functor (M : MONAD_ERROR) -> sig
  val interpret
    :  Ast.expression list
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t * value list
         , error )
         M.t
end

module MONAD_ADAPTER : sig
  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err

  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

module Interpreter : sig
  val interpret
    :  Ast.expression list
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t * value list
         , error )
         result
end
