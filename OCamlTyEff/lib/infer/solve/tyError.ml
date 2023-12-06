open! Base
open Types

type t =
  | UnificationMismatch (* XXX: better name *)
  | UnificationFail of Ty.t * Ty.t
  | UnboundVariable of string
  | OccursIn of Var.t * Ty.t
[@@deriving show {with_path= false}]
