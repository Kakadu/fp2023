(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module Assumptions = Common.Assumptions
module BoundVars = Pattern.BoundVars
module ConArityAssumpt = Common.ConArityAssumpt

open! Base

let gen str_item =
  GenMonad.run @@ Structure.gen str_item
  |> Result.map ~f:(fun ((asm, bound, ty), constrs, con_assumpt) ->
         (asm, bound, ty, constrs, con_assumpt) )
