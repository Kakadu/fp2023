(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of Typedtree.typ * Typedtree.typ

val pp_error : Format.formatter -> error -> unit
