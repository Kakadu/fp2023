(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt
  | TBool
  | TNothing
  | TArrow of typ * typ
  | TVar of int
  | TList of typ

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ
