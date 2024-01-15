(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

val close_over : Ty.t -> weak_counter:int -> Scheme.t * int
(** Close effects, rename and quantify all (not weak) variables in a type.
    Accepts weak counter to rename weaks
    and returns it's possibly modified version *)

val open_effs : Scheme.t -> Scheme.t
(**
  Open effects in a type, i.e. add polymorphic tail to all effects
  E.g. 1) [int -> int] ---> ['e. int -'e-> int]
       2) [string -[console]-> unit] ---> ['e. string -[console | 'e]-> unit]
*)
