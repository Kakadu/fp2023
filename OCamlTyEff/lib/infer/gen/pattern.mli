open! Base
open Types
open Ast

open Common

module BoundVars : sig
  (* Variables bound by pattern *)
  type t

  val fold :
    t -> init:'acc -> f:(key:Ident.t -> data:Var.t -> 'acc -> 'acc) -> 'acc

  val idents : t -> Ident.t list

  val vars : t -> Var.t list
end

val gen : pattern -> (Assumptions.t * BoundVars.t * Ty.t) GenMonad.t

val gen_many :
  pattern list -> (Assumptions.t * BoundVars.t * Ty.t list) GenMonad.t
