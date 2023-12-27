(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints
open Ast

module Assumptions = struct
  type t = (Ident.t, Var.t list, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)
  let singleton name tv = Map.singleton (module Ident) name [tv]

  let lookup = Map.find_multi
  let remove = Map.remove
  let merge = Map.merge_skewed ~combine:(fun ~key:_ v1 v2 -> List.append v1 v2)

  let idents = Map.keys
  let fold = Map.fold
end

module ConArityAssumpt = struct
  type arity = NoArgs | SomeArgs [@@deriving eq]

  type t = (Ident.t, arity, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)

  let set map con_id arity = Map.set map ~key:con_id ~data:arity
  let find = Map.find
end

let ( ! ) tv = Ty.Ty_var tv
let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, ty_res)
let ( == ) t1 t2 = Constr.EqConstr (t1, t2)
let ( ++ ) = Assumptions.merge
let ( -- ) asm = List.fold ~init:asm ~f:Assumptions.remove

let type_of_constant = function
  | Const_integer _ ->
      Ty.int
  | Const_char _ ->
      Ty.char
  | Const_string _ ->
      Ty.string
