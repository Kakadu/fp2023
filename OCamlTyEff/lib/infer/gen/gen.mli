open! Base
open Types
open Constraints
open Ast

open Common

module Assumptions : module type of Assumptions
module ConArityAssumpt : module type of ConArityAssumpt

val gen :
     structure_item
  -> (Assumptions.t * Ty.t * ConstrSet.t * ConArityAssumpt.t, TyError.t) result
