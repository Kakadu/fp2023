open! Base
open Types
open Ast

module TyError : module type of TyError

val infer_structure_item :
  Env.t -> structure_item -> (Scheme.t * Env.t, TyError.t) result
