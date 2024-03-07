(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typeandprinter

type fresh = int

module R : sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : Errorinter.error_infer -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, Errorinter.error_infer) result
end

module VarSet : sig
  type elt = fresh
  type t

  val fold_left_m : ('a -> elt -> 'a R.t) -> t -> 'a R.t -> 'a R.t
end

type binder_set = VarSet.t
type scheme = S of binder_set * typ
type environment = (string * scheme) list

val check_types
  :  ?env:environment
  -> Ast.expr list
  -> (environment * typ, Errorinter.error_infer) result
