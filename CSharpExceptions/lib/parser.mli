(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val p_number: value_ Angstrom.t
val p_string: value_ Angstrom.t
val p_char: value_ Angstrom.t
val ep_member_ident: expr Angstrom.t
val ep_method_fild: expr Angstrom.t
val ep_var_decl: expr Angstrom.t
val ep_bin_op: expr Angstrom.t


val n_parse : 'a Angstrom.t -> string -> ('a , string) result
val parse : string -> p:'a Angstrom.t -> 'a option
