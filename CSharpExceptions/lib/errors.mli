(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_check_error =
  | Double_definition_of of Ast.ident
  | Access of string
  | Other of string
  | Not_find_ident
  | Not_find_ident_of of Ast.ident
  | Method_not_find
  | Type_mismatch
  | Double_definition

val pp_type_check_error : Format.formatter -> type_check_error -> unit
val show_type_check_error : type_check_error -> string

type interpreter_error =
  | Non_existent_address
  | Type_mismatch
  | Break_error of string
  | Return_error of string
  | Runtime_error of string
  | Trying_to_assign_a_method
  | Trying_to_change_Null
  | Constructor_error of string
  | Division_by_zero
  | Using_an_uninitialized_variable
  | Methods_cannot_be_assignable
  | System_error of string
  | Non_existent_id of Ast.ident
  | Non_existent_id_
  | User_exception of Common_types.code_ident

val pp_interpreter_error : Format.formatter -> interpreter_error -> unit
val show_interpreter_error : interpreter_error -> string

type error =
  | Interpret_error of interpreter_error
  | Type_check_error of type_check_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string
