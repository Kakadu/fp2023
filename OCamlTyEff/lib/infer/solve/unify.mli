(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

val unify : Ty.t -> Ty.t -> Sub.t SolveMonad.t
