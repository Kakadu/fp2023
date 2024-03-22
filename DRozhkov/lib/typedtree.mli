(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt (* Type: Intenger*)
  | TBool (* Type: Bool*)
  | TEmpty (* Type: ()*)
  | TVar of int (* Type: var *)
  | TList of typ (* Type: list *)
  | TArrow of typ * typ (* Type: function *)

val pp_typ : Format.formatter -> typ -> unit
