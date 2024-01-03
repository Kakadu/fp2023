(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | OccursCheckFailed
  | NoVariable of id
  | UnificationFailed of ty * ty
  | IllegalRec of string
  | MatchFailed
  | EmptyInput
[@@deriving show { with_path = false }]
