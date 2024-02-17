(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt
  | TBool
  | TEmpty
  | TVar of int
  | TList of typ
  | TArrow of typ * typ

val equal_typ : typ -> typ -> bool
val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string
val pp_typ : Format.formatter -> typ -> unit
