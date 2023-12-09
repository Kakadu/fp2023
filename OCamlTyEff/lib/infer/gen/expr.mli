open! Base
open Types
open Ast

open Containers

val gen : expression -> (Assumptions.t * Ty.t) GenMonad.t
