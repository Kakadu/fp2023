open! Base
open Types
open Ast

open Common

val gen : expression -> (Assumptions.t * Ty.t) GenMonad.t
