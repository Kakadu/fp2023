open! Base
open Ast

(** Type environment. Maps identifiers to types *)
type t

val empty : t

val singleton : Ident.t -> Scheme.t -> t

val of_alist_exn : (Ident.t * Scheme.t) list -> t

val find : t -> Ident.t -> Scheme.t option

val set : t -> key:Ident.t -> data:Scheme.t -> t
