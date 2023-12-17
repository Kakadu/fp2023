(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints
open Ast

open Common

module Assumptions : module type of Assumptions
module BoundVars : module type of Pattern.BoundVars
module ConArityAssumpt : module type of ConArityAssumpt

val gen :
     structure_item
  -> ( Assumptions.t * BoundVars.t * Ty.t * ConstrSet.t * ConArityAssumpt.t
     , TyError.t )
     result
