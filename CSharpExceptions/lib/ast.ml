(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** {2 Value types} *)

type value_ =
  | Null
  | VString of string
  | VInt of int
  | VChar of char
  | VBool of bool
[@@deriving show { with_path = false }, eq]

type ident = Id of string [@@deriving show { with_path = false }, eq]

(** {2 Declarations types} *)

type type_ =
  | TMethod of meth_type
  | TVariable of var_type
[@@deriving show { with_path = false }, eq]

and meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }, eq]

(** Variable type *)
and var_type = TVar of assignable_type [@@deriving show { with_path = false }, eq]

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

and method_modifier =
  | MAccess of access_modifier
  | MStatic
(* TODO: | Virtual *)
(* TODO: | Override *)

and fild_modifier = FAccess of access_modifier
(* TODO:| New *)
(* TODO:| Const *)

and access_modifier =
  | MPublic
  | MPrivate
  | MProtected

type bin_op =
  | Asterisk (* [*] *)
  | Plus (* [+] *)
  | Minus (* [-] *)
  | Division (* [/] *)
  | Mod (* [%] *)
  | Equal (* [==] *)
  | NotEqual (* [!=] *)
  | Less (* [<] *)
  | LessOrEqual (* [+] *)
  | More (* [>] *)
  | MoreOrEqual (* [>=] *)
  | And (* [&&] *)
  | Or (* [||] *)
  | Assign (* [=] *)
[@@deriving show { with_path = false }, eq]

type un_op =
  | UMinus (* [-] *)
  | UNot (* [!] *)
  | New (* [new] *)
[@@deriving show { with_path = false }, eq]

type expr =
  | EVal of value_ (* assignable values *)
  (*  *)
  | EIdentifier of ident (* id of something e.g. class name; var name; method name *)
  | EParams of expr list (* method(a, b, c) -> [a; b; c] *)
  | EMethod_invoke of expr * expr (* method(a, b, c) -> EIdentifier, EParams *)
  (*  *)
  | EBin_op of bin_op * expr * expr
  | EUn_op of un_op * expr
  | EMember_ident of expr * expr (* access by point e.g. A.run() *)
  (* TODO: | Cast of assignable_type * expr *)
  (*  *)
  | EClass_decl of class_sign
  | EDecl of type_ * ident
  | EAssign of expr * expr
  | Steps of expr list (* sequence of actions inside {...} *)
  (*  *)
  | EIf_else of expr * expr * expr option
  | EReturn of expr option
  | EBreak
(* TODO:| EWhile *)
(* TODO:| EFor *)
(* TODO:| ETry_catch_fin *)
(* TODO:| ESwitch *)
[@@deriving show { with_path = false }, eq]

and args = Args of expr list [@@deriving show { with_path = false }, eq]

and class_member =
  | Fild of fild_sign
  | Main of method_sign * expr (* expr - Steps *)
  | Method of method_sign * expr (* expr - Steps *)
  | Constructor of constructor_sign * expr (* expr - Steps *)

and class_sign =
  { cl_modif : access_modifier option
  ; cl_id : ident
  ; parent : ident option
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
