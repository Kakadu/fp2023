(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int

type ground =
  | GInt
  | GBool
  | GUnit

val equal_ground : ground -> ground -> bool
val pp_ground : Format.formatter -> ground -> unit
val show_ground : ground -> string

type typ =
  | TVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground

val tint : typ
val tbool : typ
val tunit : typ
val tarrow : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ
val tvar : type_variable_number -> typ
val edit_numbers_in_typ : typ -> typ
val pp_type : Format.formatter -> typ -> unit
val print_typ : Format.formatter -> ?carriage:bool -> typ -> unit

type error =
  | OccursCheck
  | UnboundValue of string
  | MismatchValues of typ * typ
  | UnificationFailed of typ * typ
  | ParserAvoidedError
  | WildcardNotExpected

val pp_error : Format.formatter -> error -> unit
val print_type_error : error -> unit
