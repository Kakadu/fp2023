(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Misc

type types_arity = (Ident.t, int, Ident.comparator_witness) Map.t

type t =
  { weak_counter: int
  ; map: (Ident.t, Scheme.t, Ident.comparator_witness) Map.t
  ; types_arity: types_arity }

let get_types_arity env = env.types_arity
let set_types_arity env types_arity = {env with types_arity}

let get_weak_counter env = env.weak_counter
let set_weak_counter env weak_counter = {env with weak_counter}

let of_alist_exn list =
  { weak_counter= 1
  ; map= Map.of_alist_exn (module Ident) list
  ; types_arity= Map.empty (module Ident) }

let set env ~key ~data = {env with map= Map.set env.map ~key ~data}
let map env ~f = {env with map= Map.map env.map ~f}
let find env = Map.find env.map
let find_exn env = Map.find_exn env.map
