(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Constraints

module Sub : module type of Sub

val solve : ConstrSet.t -> (Sub.t, TyError.t) result
