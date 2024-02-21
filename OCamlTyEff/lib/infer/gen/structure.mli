(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Ast

open Common

type defined_type = {id: Ident.t; arity: int}

val gen :
     Env.types_arity
  -> structure_item
  -> ( Assumptions.t
     * Pattern.BoundVars.t
     * Ty.t option
     * Eff.t
     * defined_type list )
     GenMonad.t
