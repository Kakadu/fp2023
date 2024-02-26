(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints

(** Substitution. Maps variables to types and effects *)
type t

val empty : t
val singleton_ty : Var.t -> Ty.t -> t
val singleton_eff : Var.t -> Eff.t -> t
val mem : t -> Var.t -> bool

val apply_to_ty : t -> Ty.t -> Ty.t
(** Apply substitution to type *)

val apply_to_eff : t -> Eff.t -> Eff.t
(** Apply substitution to effect *)

val apply_to_env : t -> Env.t -> Env.t
(** Apply substitution to environment *)

val apply_to_constrs : t -> ConstrSet.t -> ConstrSet.t
(** Apply substitution to a set of constraints *)

val compose : t -> t -> t

val map_ty : f:(Ty.t -> Ty.t) -> t -> t
