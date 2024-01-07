(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | Not_find_ident
  | Not_find_ident_of of ident
  | Type_mismatch
  | Double_definition
  | Double_definition_of of ident
  | Other_error of string
  | Method_not_find
  | Access_error of string
[@@deriving show { with_path = false }]
