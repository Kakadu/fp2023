(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type parse_error = Syntax_error of string

type object_field =
  | Method
  | Variable

type infer_error =
  | Occurs_check of int * ty
  (** This error can lead to non-terminate of the type inference
      Example: [fun x -> x x]*)
  | Unbound_variable of string
  (** Variable is not defined in local context
      Example: [let increase = x + 1]*)
  | Unification_failed of ty * ty (** Unification error between two types *)
  | Several_bounds of string (** Example: [let (x, x) = 1, 3] *)
  | No_variable_rec
  (** Only variables are allowed as left-side of 'let rec'
      Example: [let rec (a, b) = 5] *)
  | Multiple_definition of object_field * string
  (** An instance variable / method must have a single definition
      Example: [object val n = 0 val n = 5 end]*)
  | Undefined_method of ty * string
  (** Occurs when sending a message to undefined method in object *)
  | Not_object of ty (** Occurs when trying to send a message to a non-object *)
  | Cannot_match_self (** A self-reference can only be a `self' variable *)

type interpreter_error =
  | Division_by_zero (** Example: [5 / 0] *)
  | Match_failure
  (** May occurs with non-exhaustive pattern-matching, if value doesn't match any case *)
  | Invalid_compare_arg of string
  (** Occurs when trying to compare two anonymous functions *)
  | Ill_right_side_rec of string (** Represents limitations of `let rec` *)
  | Ill_typed (* Unreachable with using infer *)
  | Unbound_var of string (* Unreachable with using infer *)

type error =
  | Parser of parse_error
  | Infer of infer_error
  | Interpreter of interpreter_error

(*================Constructors================*)

val occurs_check : binder * ty -> error
val unbound_variable : string -> error
val unification_failed : ty * ty -> error
val several_bounds : string -> error
val no_variable_rec : error
val multiple_variable : string -> error
val multiple_method : string -> error
val undefined_method : ty -> string -> error
val not_object : ty -> error
val cannot_match_self : error
val division_by_zero : error
val match_failure : error
val invalid_compare_arg : string -> error
val ill_right_side_rec : string -> error
val ill_typed : error
val unbound_var : string -> error
