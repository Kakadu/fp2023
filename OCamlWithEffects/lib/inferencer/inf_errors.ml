(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree

type error =
  [ `Occurs_check
    (* Trying to unify two types when one of the types contains the other type as a subtype or variable. *)
  | `Unification_failed of typ * typ (* Castable types are not compatible. *)
  | `Unbound_variable of id (* An undeclared variable is used. *)
  | `Unbound_effect of id (* An undeclared effect is used. *)
  | `Several_bounds of id (* A type variable is assigned more than one constraint. *)
  | `Wrong_effect_typ of id * typ (* The effect type in the ad is not an effect. *)
  | `Not_effect_with_args of
    id (* An effect declared without arguments attempts to pass an argument. *)
  | `Not_effect_without_args of
    id (* The effect of an argument in handler is described without an argument. *)
  | `Handler_without_continue (* There is no continuation in the effect handler. *)
  | `Handler_without_effect (* The effect handler does not contain an effect. *)
  | `Perform_with_no_effect (* The argument passed to perform is not an effect. *)
  | `Not_continue_val of
    id (* The continuation variable does not actually contain the continuation. *)
  ]

(* Constructors for types *)
let occurs_check = `Occurs_check
let unification_failed ty1 ty2 = `Unification_failed (ty1, ty2)
let unbound_variable name = `Unbound_variable name
let unbound_effect name = `Unbound_effect name
let not_reachable = `Not_reachable
let several_bounds name = `Several_bounds name
let handler_without_effect = `Handler_without_effect
let wrong_effect_type name typ = `Wrong_effect_typ (name, typ)
let not_effect_with_args name = `Not_effect_with_args name
let not_effect_without_args name = `Not_effect_without_args name
let perform_with_no_effect = `Perform_with_no_effect
let not_continue_val name = `Not_continue_val name
let handler_without_continue = `Handler_without_continue
(* ---------------- *)
