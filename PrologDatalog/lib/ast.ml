(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(**base elements of Prolog*)
type name = string [@@deriving show { with_path = false }]

type num = int [@@deriving show { with_path = false }]
type oper = string [@@deriving show { with_path = false }]
type var = string [@@deriving show { with_path = false }]

(**Simple objects of Prolog*)
type atom =
  | Name of name
  | Oper of oper
[@@deriving show { with_path = false }]

type const =
  | Num of num
  | Atom of atom
[@@deriving show { with_path = false }]

(**Structures of Prolog*)
type term =
  | Const of const
  | Var of var
  | Relation of
      { atom : atom
      ; terms : term list
      }
[@@deriving show { with_path = false }]

type many_term = Many_term of term list [@@deriving show { with_path = false }]
