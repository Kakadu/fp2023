(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Ast

module TyError : module type of TyError

val infer_structure_item :
  Env.t -> structure_item -> (Env.t * Ident.t list * Scheme.t, TyError.t) result
(**
  Infers types for structure_item.
  Returns new type environment,
  list of bound variables (with their types in returned environment)
  and type of the structure_item (type of expression for Str_eval, type of rhs for Str_value)
*)
