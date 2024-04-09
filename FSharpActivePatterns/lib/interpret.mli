(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VString of string (** "string" *)
  | VBool of bool (** true *)
  | VInt of int (** 42 *)
  | VList of value list (** [1;2;3] *)
  | VTuple of value list (** (1,2,3) *)
  | VFun of pattern * expr * (name * value) list
  | VLetWAPat of name * value (** Let without active patterns, let x y = y * 5 *)
  | VLetAPat of name list * value
  (** Let with active patterns, let (|Even|Odd|) value = .., let (|Even|_|) value = .., let (|Even|) value = ..*)
  | VCases of name (** Even, Odd, ... *)
  | VNil
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Interpret : functor (M : MonadFail) -> sig
  val eval_program : Ast.expr list -> (value, Errorinter.error_inter) M.t
end

val eval_program : expr list -> (value, Errorinter.error_inter) result
