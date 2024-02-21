(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Constraints
open Ast

open Common

module Assumptions : module type of Assumptions
module BoundVars : module type of Pattern.BoundVars
module ConArityAssumpt : module type of ConArityAssumpt

(** Describes newly defined type *)
type defined_type = Structure.defined_type = {id: Ident.t; arity: int}

val gen :
     Env.types_arity
  -> structure_item
  -> ( Assumptions.t
       * BoundVars.t
       * Ty.t option
       * Eff.t
       * ConstrSet.t
       * ConArityAssumpt.t
       * defined_type list
     , TyError.t )
     result
