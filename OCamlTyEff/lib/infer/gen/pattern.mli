open! Base
open Types
open Ast

open Containers

module BoundVars : sig
  (* Variables bound by pattern *)
  type t

  val fold :
    t -> init:'acc -> f:(key:Ident.t -> data:Var.t -> 'acc -> 'acc) -> 'acc

  val idents : t -> Ident.t list
end

val gen : pattern -> (Assumptions.t * BoundVars.t * Ty.t) GenMonad.t
