(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** {2 Value types} *)

type value_ =
  | Null
  | VString of string
  | VInt of int
  | VChar of char
  | VBool of bool
[@@deriving show { with_path = false }]

type ident = Id of string [@@deriving show { with_path = false }]

(** {2 Declarations types} *)

(** Variable type *)

(** Type that can be assigne *)

type base_type =
  | TInt
  | TChar
  | TBool
[@@deriving show { with_path = false }]

type nulable_type =
  | TBase of base_type
  | TString
  | TClass of ident
[@@deriving show { with_path = false }]

type assignable_type =
  | TNot_Nullable of base_type
  | TNullable of nulable_type
[@@deriving show { with_path = false }]

type var_type = TVar of assignable_type [@@deriving show { with_path = false }]

type meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }]

type access_modifier =
  | MPublic
  | MPrivate
  | MProtected
[@@deriving show { with_path = false }]

type method_modifier =
  | MAccess of access_modifier
  | MStatic
[@@deriving show { with_path = false }]

type fild_modifier = FAccess of access_modifier
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

type un_op =
  | UMinus (* [-] *)
  | UNot (* [!] *)
  | New (* [new] *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of value_ (* assignable values *)
  (*  *)
  | EIdentifier of ident (* id of something e.g. class name; var name; method name *)
  | EMethod_invoke of expr * params (* method(a, b, c) | Class.method(a, b, c) *)
  (*  *)
  | EPoint_access of expr * expr (* access by point e.g. A.run() *)
  | EBin_op of bin_op * expr * expr
  | EUn_op of un_op * expr
(*  *)

and params = Params of expr list [@@deriving show { with_path = false }]

type var_decl = Var_decl of var_type * ident [@@deriving show { with_path = false }]
type args = Args of var_decl list [@@deriving show { with_path = false }]

type statement =
  | SExpr of expr
  | Steps of statement list
  | SIf_else of expr * statement * statement option
  | SDecl of var_decl * expr option
  | SReturn of expr option
  | SThrow of expr option
  | SBreak
  | SWhile of expr * statement
  | SFor of
      { f_init_p : statement option
      ; f_cond_p : expr option
      ; f_iter_p : expr option
      ; f_body : statement
      }
  | STry_catch_fin of
      { try_s : statement
      ; catch_s : ((var_decl * expr option) option * statement) option
      ; finally_s : statement option
      }
[@@deriving show { with_path = false }]

type fild_sign =
  { f_modif : fild_modifier option
  ; f_type : var_type
  ; f_id : ident
  }
[@@deriving show { with_path = false }]

type method_sign =
  { m_modif : method_modifier option
  ; m_type : meth_type
  ; m_id : ident
  ; m_args : args
  }
[@@deriving show { with_path = false }]

type constructor_sign =
  { con_modif : access_modifier option
  ; con_id : ident
  ; con_args : args
  ; base_params : params option
  }
[@@deriving show { with_path = false }]

type class_member =
  | Fild of fild_sign * expr option
  | Main of statement
  | Method of method_sign * statement (* statment - Steps *)
  | Constructor of constructor_sign * statement (* statment - Steps *)
[@@deriving show { with_path = false }]

type class_decl =
  { cl_modif : access_modifier option
  ; cl_id : ident
  ; parent : ident option
  ; cl_mems : class_member list
  }
[@@deriving show { with_path = false }]

type tast = Ast of class_decl list [@@deriving show { with_path = false }]
