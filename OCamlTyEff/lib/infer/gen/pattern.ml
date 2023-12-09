open! Base
open Types
open Ast

open Common
open Containers
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

let rec gen = function
  | Pat_var name ->
      let* var = fresh_var in
      return (As.singleton (Ident name) var, !var)
  | Pat_any ->
      let* var = fresh_var in
      return (As.empty, !var)
  | Pat_constant const ->
      return (As.empty, type_of_constant const)
  | Pat_tuple pats ->
      let* asm, tys = gen_many gen pats in
      return (asm, Ty.Ty_tuple tys)
  | Pat_construct (_, _) ->
      failwith "not implemented"
  | Pat_or (_, _) ->
      failwith "not implemented"
