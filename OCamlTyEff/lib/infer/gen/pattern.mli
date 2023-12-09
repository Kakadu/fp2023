open! Base
open Types
open Ast

open Containers

val gen : pattern -> (Assumptions.t * Ty.t) GenMonad.t
