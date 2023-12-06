open! Base
open Types
open Ast

val infer_expr : Env.t -> expression -> (Ty.t, TyError.t) result
