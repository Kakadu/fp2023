(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value_ =
  | Null
  | VString of string
  | VClass of string (*а оно надо вообще?*)
  | VInt of int
  | VChar of char
  | VBool of bool
[@@deriving show { with_path = false }, eq]

type ident = Name of string
[@@deriving show { with_path = false }, eq]

(* ************************************************* *)
type type_ =
  | Method of meth_type
  | Variable of var_type (** Method type *)
[@@deriving show { with_path = false }, eq]

and meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }, eq]

(** Variable type *)
and var_type =
  (* TODO: *)
  (* | Var *)
  | TVariable of assignable_type
[@@deriving show { with_path = false }, eq]

(** Type that can be assigne *)
and assignable_type =
  | TNot_Nullable of base_type
  | TNullable of nulable_type
[@@deriving show { with_path = false }, eq]

and base_type =
  | TInt
  | TChar
  | TBool
[@@deriving show { with_path = false }, eq]

and nulable_type =
  | TBase of base_type
  | TString
  | TClass of ident
[@@deriving show { with_path = false }, eq]

(* ************************************************* *)
type modifier =
  | Class of access_modifier
  | Method of method_modifier
  | Fild of fild_modifier

and method_modifier =
  | MAccess of access_modifier
  | Static
(* | Virtual *)
(* | Override *)

and fild_modifier = FAccess of access_modifier
(* | New *)
(* | Const *)

and access_modifier =
  | Public
  | Private
  | Protected

(* ************************************************* *)
type bin_op =
  | Asterisk
  | Plus
  | Minus
  | Division
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | More
  | MoreOrEqual
  | And
  | Or

type un_op =
  | UMinus
  | UNot

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type expr =
  | EVal of value_
  (*  *)
  | EIdentifier of ident
  | EParams of expr list
  | EMethod_invoke of expr * expr
  (*  *)
  | EBin_op of bin_op * expr * expr
  | EUn_op of un_op * expr
  | Return of expr option
  | EMember_ident of expr * expr
  | Cast of assignable_type * expr
  | Assign of expr * expr (* ? *)
  (*  *)
  | EClass_decl of class_
  | EException_decl of class_
  | EClass_member of class_member
  | EVar_decl of var_type * ident
  | Steps of expr list
  (*  *)
  | EIf_else of expr * expr * expr option
(* | EWhile *)
(* | EFor *)
(* | ETry_catch_fin *)
(* | EBreak *)
(* | ESwitch *)

and args = (ident * expr) list

(* TODO: переделать на records? *)
and method_sign =
  | Main
  | Default of method_modifier list option * meth_type * ident * args option
  | Constructor of args option * expr option

and class_member =
  | Fild of fild_modifier list option * fild_modifier * ident * expr option
  | Method of method_sign * expr option

and class_ = Class of access_modifier option * ident * expr option

(* Application [f g ] *)
(** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)
