(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type atom =
  | Name of string
  | Oper of string

val equal_atom : atom -> atom -> bool
val pp_atom : Format.formatter -> atom -> unit
val show_atom : atom -> string

type const =
  | Num of int
  | Atom of atom

val equal_const : const -> const -> bool
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type term =
  | Const of const
  | Var of string
  | Relation of
      { atom : atom
      ; terms : term list
      }

val equal_term : term -> term -> bool
val pp_term : Format.formatter -> term -> unit
val show_term : term -> string

type many_term = Many_term of term list

val equal_many_term : many_term -> many_term -> bool
val pp_many_term : Format.formatter -> many_term -> unit
val show_many_term : many_term -> string
