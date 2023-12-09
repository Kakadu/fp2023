open! Base
open Types
open Ast

open Containers

val gen : structure_item -> (Assumptions.t * Ty.t) GenMonad.t
