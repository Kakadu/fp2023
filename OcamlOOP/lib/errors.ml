(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type parse_error = Syntax_error of string

type object_field =
  | Method
  | Variable

type infer_error =
  | Occurs_check of int * ty
  | Unbound_variable of string
  | Unification_failed of ty * ty
  | Several_bounds of string
  | No_variable_rec
  | Multiple_definition of object_field * string
  | Undefined_method of ty * string
  | Not_object of ty
  | Cannot_match_self

type interpreter_error =
  | Division_by_zero
  | Match_failure
  | Invalid_compare_arg of string
  | Ill_right_side_rec of string
  | Ill_typed
  | Unbound_var of string

type error =
  | Parser of parse_error
  | Infer of infer_error
  | Interpreter of interpreter_error

let occurs_check (b, t) = Infer (Occurs_check (b, t))
let unbound_variable v = Infer (Unbound_variable v)
let unification_failed (t1, t2) = Infer (Unification_failed (t1, t2))
let several_bounds v = Infer (Several_bounds v)
let no_variable_rec = Infer No_variable_rec
let multiple_variable name = Infer (Multiple_definition (Variable, name))
let multiple_method name = Infer (Multiple_definition (Method, name))
let undefined_method ty_obj name = Infer (Undefined_method (ty_obj, name))
let not_object t = Infer (Not_object t)
let cannot_match_self = Infer Cannot_match_self
let division_by_zero = Interpreter Division_by_zero
let match_failure = Interpreter Match_failure
let invalid_compare_arg s = Interpreter (Invalid_compare_arg s)
let ill_right_side_rec s = Interpreter (Ill_right_side_rec s)
let ill_typed = Interpreter Ill_typed
let unbound_var s = Interpreter (Unbound_var s)
