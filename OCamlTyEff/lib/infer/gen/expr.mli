(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Ast

open Common

val gen : expression -> (Assumptions.t * Ty.t * Eff.t) GenMonad.t
