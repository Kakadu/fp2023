(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* type value_ =
  | VNullable of nulable_value
  | VNot_Nullable of base_value *)

(* and nulable_value = *)
type value_ =
  | Null
  | VBase of base_value
  | VString of string
  | VArray of value_ list
  | VClass of string
[@@deriving show]

and base_value =
  | VInt of int
  | VChar of char
  | VBool of bool
  | VDouble of float
[@@deriving show]

type value_literal = 
  | Double_lit
  (* | Float_lit  *)


(* ************************************************* *)
type arity = Arity of int

type type_ =
  | Method of meth_type
  | Variable of var_type (** Method type *)

and meth_type =
  | Void
  | TReturn of assignable_type

(** Variable type *)
and var_type =
  (* TODO: *)
  (* | Var *)
  | TVariable of assignable_type

(** Type that can be assigne *)
and assignable_type =
  | TNot_Nullable of base_type
  | TNullable of nulable_type

and base_type =
  | TInt
  | TChar
  | TBool
  | TDouble

and nulable_type =
  | TBase of base_type
  | TArray of arity * var_type
  | TString
  | TClass of string

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

type ident = Name of string

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
  | Assign of expr * expr
  (*  *)
  | EClass_decl of class_
  | EException_decl of class_
  | EClass_member of class_member
  | EVar_decl of var_type * expr
  | Steps of expr list
  | EHas_member of ident * expr (** A.b *)
  (*  *)
  | EIf_else of expr * expr * expr option
(* | EWhile *)
(* | EFor *)
(* | ETry_catch_fin *)
(* | EBreak *)
(* | ESwitch *)

and args = (ident * expr) list

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
