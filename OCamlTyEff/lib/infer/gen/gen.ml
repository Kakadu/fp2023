(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module Assumptions = Common.Assumptions
module BoundVars = Pattern.BoundVars
module ConArityAssumpt = Common.ConArityAssumpt

open! Base
open Misc
open Common

type defined_type = Structure.defined_type = {id: Ident.t; arity: int}

let gen types_arity str_item =
  GenMonad.run @@ Structure.gen types_arity str_item
  |> Result.map
       ~f:(fun ((asm, bound, ty, eff, defined_types), constrs, con_assumpt) ->
         (asm, bound, ty, eff, constrs, con_assumpt, defined_types) )
