(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string

val pp_name : Format.formatter -> name -> unit
val show_name : name -> string

type num = int

val pp_num : Format.formatter -> num -> unit
val show_num : num -> string

type oper = string

val pp_oper : Format.formatter -> oper -> unit
val show_oper : oper -> string

type var = string

val pp_var : Format.formatter -> var -> unit
val show_var : var -> string

type atom =
  | Name of name
  | Oper of oper

val pp_atom : Format.formatter -> atom -> unit
val show_atom : atom -> string

type const =
  | Num of num
  | Atom of atom

val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type term =
  | Const of const
  | Var of var
  | Relation of
      { atom : atom
      ; terms : term list
      }

val pp_term : Format.formatter -> term -> unit
val show_term : term -> string

type many_term = Many_term of term list

val pp_many_term : Format.formatter -> many_term -> unit
val show_many_term : many_term -> string
