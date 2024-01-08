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
  | Double_definition_of of ident
  | User_exception of address
  | Access_error of string
  | Other_error of string
  | Non_existent_address
[@@deriving show { with_path = false }]
