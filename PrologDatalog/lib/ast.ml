(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]
type num = int [@@deriving eq, show { with_path = false }]
type oper = string [@@deriving eq, show { with_path = false }]

type atom =
  | Name of name
  | Oper of oper
[@@deriving eq, show { with_path = false }]

type const =
  | Num of num
  | Atom of atom
[@@deriving eq, show { with_path = false }]

type term =
  | Const of const
  | Var of string
  | Relation of
      { atom : atom
      ; terms : term list
      }
[@@deriving eq, show { with_path = false }]

type many_term = Many_term of term list [@@deriving eq, show { with_path = false }]
