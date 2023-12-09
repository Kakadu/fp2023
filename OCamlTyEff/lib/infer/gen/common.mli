open! Base
open Types
open Constraints
open Ast

open Containers

val ( ! ) : Var.t -> Ty.t

val ( @> ) : Ty.t -> Ty.t -> Ty.t

val ( == ) : Ty.t -> Ty.t -> Constr.t

val ( ++ ) : Assumptions.t -> Assumptions.t -> Assumptions.t

val ( -- ) : Assumptions.t -> Ident.t list -> Assumptions.t

val type_of_constant : constant -> Ty.t

val gen_many :
     ('a -> (Assumptions.t * Ty.t) GenMonad.t)
  -> 'a list
  -> (Assumptions.t * Ty.t list) GenMonad.t
