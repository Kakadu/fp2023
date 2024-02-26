(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Ast

module TyError : module type of TyError

val infer_structure_item :
     ?rec_types:bool
  -> Env.t
  -> structure_item
  -> (Env.t * Ident.t list * Scheme.t option, TyError.t) result
(**
  Infers types for structure_item.
  Returns new type environment,
  list of bound variables (with their types in returned environment)
  and type of the structure_item if applicable
*)
