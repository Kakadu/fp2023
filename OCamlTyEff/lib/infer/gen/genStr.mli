open! Base
open Types
open Ast

val gen_str : structure_item -> (Assumptions.t * Ty.t) GenMonad.t
