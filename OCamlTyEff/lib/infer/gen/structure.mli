open! Base
open Types
open Ast

open Common

val gen :
  structure_item -> (Assumptions.t * Pattern.BoundVars.t * Ty.t) GenMonad.t
