(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt (** int type *)
  | TBool (** bools type *)
  | TArrow of typ * typ (** type -> type *)
  | TVar of int (** var type *)
  | TUnit (** unit *)
  | TList of typ (* list type *)

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ
