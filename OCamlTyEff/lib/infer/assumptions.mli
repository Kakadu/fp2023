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
