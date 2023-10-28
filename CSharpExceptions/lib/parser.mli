(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val p_number: value_ Angstrom.t
val p_string: value_ Angstrom.t
val p_char: value_ Angstrom.t
val ep_var_decl: expr Angstrom.t
val ep_operation: expr Angstrom.t
val ep_eAssign_eDecl: expr Angstrom.t
val ep_brunch_loop: expr Angstrom.t
val ep_steps: expr Angstrom.t
val _ep_if_cond: expr Angstrom.t
val _ep_keyword: string -> string Angstrom.t


val n_parse : 'a Angstrom.t -> string -> ('a , string) result
val parse : string -> p:'a Angstrom.t -> 'a option
