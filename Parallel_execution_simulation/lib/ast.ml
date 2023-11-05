(** Copyright 2023-2024, Alexandra Lanovaya*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type register = Register of string [@@deriving show]
type shared_variable = SharedVariable of string [@@deriving show]
type barrier = Barrier [@@deriving show]

type expr =
  | Constant of int
  | Read of register
  | ReadShared of shared_variable
  | Write of shared_variable * expr
  | Assign of register * expr
  | Arith of string * expr * expr
[@@deriving show]

type thread = (barrier option * expr * barrier option) list [@@deriving show]
type ast = thread list [@@deriving show]
