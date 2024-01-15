(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Monads.Std
open Misc
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

  val idents : t -> Ident.t list
  val fold :
    t -> init:'acc -> f:(key:Ident.t -> data:Var.t list -> 'acc -> 'acc) -> 'acc
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

  val set : t -> Ident.t -> arity -> t
  val find : t -> Ident.t -> arity option
end

module GenMonad : sig
  include Monad.S

  val run : 'a t -> ('a * ConstrSet.t * ConArityAssumpt.t, TyError.t) result

  module Gen : sig
    val varset : VarSet.t t
    val extend_varset : Var.t list -> 'a t -> 'a t

    val add_constrs : Constr.t list -> unit t
    val add_con_assumpt : Ident.t -> ConArityAssumpt.arity -> unit t

    val fresh_var : Var.t t
    val fresh_eff : Eff.t t

    val fail : TyError.t -> 'a t
  end
end

val ( ! ) : Var.t -> Ty.t
val ( == ) : Ty.t -> Ty.t -> Constr.t
val ( === ) : Eff.t -> Eff.t -> Constr.t
val ( ++ ) : Assumptions.t -> Assumptions.t -> Assumptions.t
val ( -- ) : Assumptions.t -> Ident.t list -> Assumptions.t

val type_of_constant : constant -> Ty.t

val check_rec_rhs : Ident.t -> expression -> unit GenMonad.t
