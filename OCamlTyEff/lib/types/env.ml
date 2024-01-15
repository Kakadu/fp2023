(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Tys
open Misc

type t =
  {weak_counter: int; map: (Ident.t, Scheme.t, Ident.comparator_witness) Map.t}

let of_alist_exn ~weak_counter alist =
  {weak_counter; map= Map.of_alist_exn (module Ident) alist}

let get_weak_counter env = env.weak_counter
let set_weak_counter env weak_counter = {env with weak_counter}

let set env ~key ~data = {env with map= Map.set env.map ~key ~data}
let map env ~f = {env with map= Map.map env.map ~f}
let find env = Map.find env.map
let find_exn env = Map.find_exn env.map
