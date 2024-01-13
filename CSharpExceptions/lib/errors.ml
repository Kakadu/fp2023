(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Env_types.Common_env

type error =
  | Not_find_ident
  | Not_find_ident_of of ident
  | Method_not_find
  | Type_mismatch
  | Double_definition
  (*  *)
  | Non_existent_address
  | Break_error of string
  | Return_error of string
  | Runtime_error of string
  | Using_an_uninitialized_variable
  | Trying_to_change_Null
  | Constructor_error of string
  | Division_by_zero
  | Methods_cannot_be_assignable
  | System_error of string
  (*  *)
  | Double_definition_of of ident
  | User_exception of address
  | Access_error of string
  | Other_error of string
  | Type_check_error of string
[@@deriving show { with_path = false }]
