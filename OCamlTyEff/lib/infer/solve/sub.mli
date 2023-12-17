(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints

(** Substitution. Maps type variables to types *)
type t

val empty : t

val singleton : Var.t -> Ty.t -> t

val apply : t -> Ty.t -> Ty.t
(** Apply substitution to type *)

val apply_to_constrs : t -> ConstrSet.t -> ConstrSet.t
(** Apply substitution to a set of constraints *)

val compose : t -> t -> t
