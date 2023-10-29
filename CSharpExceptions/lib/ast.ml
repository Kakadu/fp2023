(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value_ =
  | Null
  | VString of string
  | VInt of int
  | VChar of char
  | VBool of bool
  | VVar of string
[@@deriving show { with_path = false }, eq]

type ident = Id of string [@@deriving show { with_path = false }, eq]

(* ************************************************* *)
type type_ =
  | TMethod of meth_type
  | TVariable of var_type (** Method type *)
[@@deriving show { with_path = false }, eq]

and meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }, eq]

(** Variable type *)
and var_type =
  (* TODO: *)
  (* | Var *)
  | TVar of assignable_type
[@@deriving show { with_path = false }, eq]

(** Type that can be assigne *)
and assignable_type =
  | TNot_Nullable of base_type
  | TNullable of nulable_type

and base_type =
  | TInt
  | TChar
  | TBool

and nulable_type =
  | TBase of base_type
  | TString
  | TClass of ident

(* ************************************************* *)
(* type modifier =
   | MClass of access_modifier
   | MMethod of method_modifier
   | MFild of fild_modifier *)
(* [@@deriving show { with_path = false }, eq] *)
and method_modifier =
  | MAccess of access_modifier
  | MStatic
(* | Virtual *)
(* | Override *)

and fild_modifier = FAccess of access_modifier
(* | New *)
(* | Const *)

and access_modifier =
  | MPublic
  | MPrivate
  | MProtected

(* ************************************************* *)
type bin_op =
  | Asterisk
  | Plus
  | Minus
  | Division
  | Mod
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | More
  | MoreOrEqual
  | And
  | Or
  | Assign
[@@deriving show { with_path = false }, eq]

type un_op =
  | UMinus
  | UNot
  | New
[@@deriving show { with_path = false }, eq]

(* The main type for our AST *)
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
  (*  *)
  | EClass_decl of class_sign
  | EDecl of type_ * ident
  | EAssign of expr * expr
  | Steps of expr list
  (*  *)
  | EIf_else of expr * expr * expr option
  | EReturn of expr option
  | EBreak
(* | EWhile *)
(* | EFor *)
(* | ETry_catch_fin *)
(* | ESwitch *)
[@@deriving show { with_path = false }, eq]

and args = Args of expr list [@@deriving show { with_path = false }, eq]

and class_member =
  | Fild of fild_sign
  | Main of method_sign * expr
  | Method of method_sign * expr (* <- для юли expr option, если она делает abstruct *)
  | Constructor of constructor_sign * expr

and class_sign =
  { cl_modif : access_modifier option
  ; cl_id : ident
  ; parent : ident option (* <- для юли мб понадобится еще как-то методы обозначить*)
  ; cl_mems : class_member list
  }

and fild_sign =
  { f_modif : fild_modifier option
  ; f_type : var_type
  ; f_id : ident
  ; f_val : expr option
  }

and method_sign =
  { m_modif : method_modifier option
  ; m_type : meth_type
  ; m_id : ident
  ; m_args : args
  }

and constructor_sign =
  { con_modif : access_modifier option
  ; con_id : ident
  ; con_args : args
  ; base_params : expr option
  }

type tast = class_sign list [@@deriving show { with_path = false }, eq]
