(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Ast

module Val : module type of Values.Val

module Bounds : module type of Values.Bounds
module Env : module type of Values.Env

module EvalMonad : module type of Values.EvalMonad
module EvalError : module type of Values.EvalError

val eval_structure_item :
     printer:(string -> unit)
  -> Env.t
  -> structure_item
  -> (Env.t * Ident.t list * Val.t option, EvalError.t) result
(**
  Evaluates structure_item.
  [printer] is used by built-in functions to print values.
  Returns new eval environment,
  list of bound variables (with their values in returned environment)
  and value of the structure_item if applicable
*)
