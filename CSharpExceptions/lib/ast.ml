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

(** Variable type *)

(** Type that can be assigne *)

type base_type =
  | TInt
  | TChar
  | TBool
[@@deriving show { with_path = false }, eq]

type nulable_type =
  | TBase of base_type
  | TString
  | TClass of ident
[@@deriving show { with_path = false }, eq]

type assignable_type =
  | TNot_Nullable of base_type
  | TNullable of nulable_type
[@@deriving show { with_path = false }, eq]

type var_type = TVar of assignable_type [@@deriving show { with_path = false }, eq]

type meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }, eq]

type type_ =
  | TMethod of meth_type
  | TVariable of var_type
[@@deriving show { with_path = false }, eq]

type access_modifier =
  | MPublic
  | MPrivate
  | MProtected
[@@deriving show { with_path = false }, eq]

type method_modifier =
  | MAccess of access_modifier
  | MStatic
(* TODO: | Virtual *)
(* TODO: | Override *)
[@@deriving show { with_path = false }, eq]

type fild_modifier = FAccess of access_modifier
(* TODO:| New *)
(* TODO:| Const *)
[@@deriving show { with_path = false }, eq]

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
  | EConst of value_ (* assignable values *)
  (*  *)
  | EIdentifier of ident (* id of something e.g. class name; var name; method name *)
  | EMethod_invoke of expr * params (* method(a, b, c) | Class.method(a, b, c) *)
  (*  *)
  | EBin_op of bin_op * expr * expr
  | EPoint_access of expr * expr (* access by point e.g. A.run() *)
  | EUn_op of un_op * expr
(*  *)
(* TODO: | Cast of assignable_type * expr *)

and params = Params of expr list [@@deriving show { with_path = false }, eq]

type var_decl = Var_decl of type_ * ident [@@deriving show { with_path = false }, eq]
type args = Args of var_decl list [@@deriving show { with_path = false }, eq]

type statement =
  | SExpr of expr
  | Steps of statement list (* sequence of actions inside {...} *)
  | SIf_else of expr * statement * statement option
  | SDecl of var_decl * expr option
  | SReturn of expr option
  | SBreak
[@@deriving show { with_path = false }, eq]

(* TODO:| SWhile *)
(* TODO:| SFor *)
(* TODO:| STry_catch_fin + throw...*)
(* TODO:| Switch *)
type fild_sign =
  { f_modif : fild_modifier option
  ; f_type : var_type
  ; f_id : ident
  ; f_val : expr option
  }
[@@deriving show { with_path = false }, eq]

type method_sign =
  { m_modif : method_modifier option
  ; m_type : meth_type
  ; m_id : ident
  ; m_args : args
  }
[@@deriving show { with_path = false }, eq]

type constructor_sign =
  { con_modif : access_modifier option
  ; con_id : ident
  ; con_args : args
  ; base_params : params option
  }
[@@deriving show { with_path = false }, eq]

type class_member =
  | Fild of fild_sign
  | Main of statement
  | Method of method_sign * statement (* statment - Steps *)
  | Constructor of constructor_sign * statement (* statment - Steps *)
[@@deriving show { with_path = false }, eq]

type class_decl =
  {
    cl_modif : access_modifier option
  ; cl_id : ident
  ; parent : ident option
  ; cl_mems : class_member list
  }
[@@deriving show { with_path = false }, eq]

type tast = Ast of class_decl list [@@deriving show { with_path = false }, eq]
