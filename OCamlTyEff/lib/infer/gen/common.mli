(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints
open Ast

module Assumptions : sig
  (**
    Assumptions about identifiers.
    Maps identifiers to a list of type variables
    that represent identifier's supposed type.
  *)
  type t

  val empty : t

  val singleton : Ident.t -> Var.t -> t

  val lookup : t -> Ident.t -> Var.t list

  val remove : t -> Ident.t -> t

  val merge : t -> t -> t

  val fold :
    t -> init:'acc -> f:(key:Ident.t -> data:Var.t list -> 'acc -> 'acc) -> 'acc

  val idents : t -> Ident.t list
end

module ConArityAssumpt : sig
  type arity = NoArgs | SomeArgs

  val equal_arity : arity -> arity -> bool

  (**
    Assumptions about constructors arity.
    Added when inferring constructor application.
    Later used to verify that assumed arity
    matches arity of constructor in type environment )
  *)
  type t

  val empty : t

  val find : t -> Ident.t -> arity option

  val set : t -> Ident.t -> arity -> t
end

val ( ! ) : Var.t -> Ty.t

val ( @> ) : Ty.t -> Ty.t -> Ty.t

val ( == ) : Ty.t -> Ty.t -> Constr.t

val ( ++ ) : Assumptions.t -> Assumptions.t -> Assumptions.t

val ( -- ) : Assumptions.t -> Ident.t list -> Assumptions.t

val type_of_constant : constant -> Ty.t
