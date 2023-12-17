(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Ast

open Common

module BoundVars : sig
  (**
    Variables bound by patterns.
    Maps identifiers of bound variables to respective type variables
  *)
  type t

  val empty : t

  val singleton : Ident.t -> Var.t -> t

  val fold :
    t -> init:'acc -> f:(key:Ident.t -> data:Var.t -> 'acc -> 'acc) -> 'acc

  val idents : t -> Ident.t list

  val vars : t -> Var.t list
end

val gen : pattern -> (Assumptions.t * BoundVars.t * Ty.t) GenMonad.t

val gen_many :
  pattern list -> (Assumptions.t * BoundVars.t * Ty.t list) GenMonad.t
