open! Base
open Ast

(** Type environment. Maps identifiers to types *)
type t

val empty : t

val singleton : Ident.t -> Scheme.t -> t