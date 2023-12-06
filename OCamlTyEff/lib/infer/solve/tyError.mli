open! Base
open Types

type t =
  | UnificationMismatch
  | UnificationFail of Ty.t * Ty.t
  | UnboundVariable of string
  | OccursIn of Var.t * Ty.t

val pp : Format.formatter -> t -> unit