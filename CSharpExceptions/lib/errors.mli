(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_check_error =
  | Double_definition of Ast.ident
  | Not_find_ident of Ast.ident
  | Type_mismatch
  | Access of string
  (** Errors related to access (for example, accessing a private field) *)
  | Other of string

val pp_type_check_error : Format.formatter -> type_check_error -> unit
val show_type_check_error : type_check_error -> string

type interpreter_error =
  | Runtime_error of string
  | Type_mismatch
  | Null_reference
  | Uninitialized_variable
  | Syntax_error of string
  | Division_by_zero
  | System_error of string (** Specific internal interpreter errors *)
  | User_exception of Common_types.code_ident
  (** If the interpreter's return result is a thrown exception *)

val pp_interpreter_error : Format.formatter -> interpreter_error -> unit
val show_interpreter_error : interpreter_error -> string

type error =
  | Interpret_error of interpreter_error
  | Type_check_error of type_check_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string
