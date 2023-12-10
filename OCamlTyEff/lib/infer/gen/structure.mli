open! Base
open Types
open Ast

open Common

val gen : structure_item -> (Assumptions.t * Ty.t) GenMonad.t
