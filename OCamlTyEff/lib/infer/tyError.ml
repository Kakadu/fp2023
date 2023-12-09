open! Base
open Types
open Ast

type t =
  | UnificationMismatch (* XXX: better name *)
  | UnificationFail of Ty.t * Ty.t
  | UnboundVariable of Ident.t
  | OccursIn of Var.t * Ty.t
  | PatVarBoundSeveralTimes of Ident.t
  | ConstructorArityMismatch of Ident.t
[@@deriving show {with_path= false}]
