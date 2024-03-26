(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typeandprinter

type fresh = int

module R : sig
  type 'a t
end

module VarSet : sig
  type elt
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

val run_infer : ('a * typ, Errorinter.error_infer) result -> unit
