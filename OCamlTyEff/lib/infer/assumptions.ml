open! Base
open Types
open Ast

type t = (Ident.t, Var.t list, Ident.comparator_witness) Map.t

let empty = Map.empty (module Ident)

let singleton name tv = Map.singleton (module Ident) name [tv]
