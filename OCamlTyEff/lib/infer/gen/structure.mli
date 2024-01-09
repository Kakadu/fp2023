(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Ast

open Common

val gen :
     structure_item
  -> (Assumptions.t * Pattern.BoundVars.t * Ty.t * Eff.t) GenMonad.t
