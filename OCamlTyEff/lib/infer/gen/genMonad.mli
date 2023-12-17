(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Monads.Std
open Types
open Ast

open Constraints
open Common

include Monad.S

val run : 'a t -> ('a * ConstrSet.t * ConArityAssumpt.t, TyError.t) result

module Gen : sig
  val varset : VarSet.t t

  val extend_varset : Var.t list -> 'a t -> 'a t

  val add_constrs : Constr.t list -> unit t

  val add_con_assumpt : Ident.t -> ConArityAssumpt.arity -> unit t

  val fresh_var : Var.t t

  val fail : TyError.t -> 'a t
end
