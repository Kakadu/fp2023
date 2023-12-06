open! Base
open Types
open Ast

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
