(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s = 
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
end

type binder = int [@@deriving eq, show {with_path = false}]

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty = 
  | TPrim of string (** ground type: int, bool *)
  | TVar of binder (** var: 'a, 'b *)
  | TTuple of ty list
  | TArrow of ty * ty (** function: bool -> int *)
  | TList of ty (** 'a list *)
[@@deriving show {with_path = false}]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

type error =
  [ `Occurs_check of int * ty
    (** This error can lead to non-determinism of the type inference
        Example: [fun x -> x x]*)
  | `Unbound_variable of string
    (** Variable is not defined in local context
        Example: [let increase = x + 1]*)
  | `Unification_failed of ty * ty (** Unification error between two types *)
  | `Several_bounds of string (** Example: let (x, x) = 1, 3 *)
  | `Unreachable
    (** For unreachable cases like missing cases in pattern-matching, because parser doesn't recognize this construction *)
  | `Not_implemented (** Temporary *)
  | `No_variable_rec
    (** Only variables are allowed as left-side of 'let rec'
        Example: [let rec (a, b) = 5] *)
  ]


let tint = TPrim "int"
let tbool = TPrim "bool"
let tarrow l r = TArrow (l, r)
let tvar x = TVar x 
let tlist x = TList x
let ttuple ts = TTuple ts
