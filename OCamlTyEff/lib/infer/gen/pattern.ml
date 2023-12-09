open! Base
open Types
open Ast

open Common
open Containers
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

module BoundVars = struct
  type t = (Ident.t, Var.t, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)

  let singleton = Map.singleton (module Ident)

  let fold = Map.fold

  let idents = Map.keys

  exception Rebound of Ident.t
  let merge m1 m2 =
    try
      return
      @@ Map.merge_skewed m1 m2 ~combine:(fun ~key:id _ _ ->
             raise @@ Rebound id )
    with Rebound id -> fail @@ PatVarBoundSeveralTimes id
end

let rec gen = function
  | Pat_var name ->
      let* var = fresh_var in
      return (As.empty, BoundVars.singleton (Ident name) var, !var)
  | Pat_any ->
      let* var = fresh_var in
      return (As.empty, BoundVars.empty, !var)
  | Pat_constant const ->
      return (As.empty, BoundVars.empty, type_of_constant const)
  | Pat_tuple pats ->
      let* asm, bound_vars, tys = gen_many pats in
      return (asm, bound_vars, Ty.Ty_tuple tys)
  | Pat_construct (_, _) ->
      failwith "not implemented"
  | Pat_or (_, _) ->
      failwith "not implemented"

and gen_many =
  GenMonad.List.fold ~init:(As.empty, BoundVars.empty, []) ~f:(fun acc pat ->
      let* asm, bound, ty = gen pat in
      let acc_asm, acc_bound, acc_tys = acc in

      let* new_bound = BoundVars.merge acc_bound bound in
      return (acc_asm ++ asm, new_bound, ty :: acc_tys) )
