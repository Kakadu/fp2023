(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type type_check_error =
  | Double_definition of ident
  | Not_find_ident of ident
  | Type_mismatch
  | Access of string
  | Other of string
[@@deriving show { with_path = false }]

type interpreter_error =
  | Runtime_error of string
  | Type_mismatch
  | Null_reference
  | Uninitialized_variable
  | Syntax_error of string
  | Division_by_zero
  | System_error of string
  | User_exception of Common_types.code_ident
[@@deriving show { with_path = false }]

type error =
  | Interpret_error of interpreter_error
  | Type_check_error of type_check_error
[@@deriving show { with_path = false }]
