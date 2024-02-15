(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** {2 Value types} *)

type value_ =
  | Null
  | VString of string (** Operations on type are not supported *)
  | VInt of int (** Only arithmetic operations are supported *)
  | VChar of char (** Operations on type are not supported *)
  | VBool of bool (** Only logical operations are supported *)
[@@deriving show { with_path = false }]

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

type var_type = TVar of assignable_type [@@deriving show { with_path = false }]

type meth_type =
  | Void
  | TReturn of assignable_type
[@@deriving show { with_path = false }, eq]

type access_modifier =
  | MPublic
  | MPrivate
  | MProtected (** UNSOPPORTED: rudimental, can be used as a MPrivate *)
[@@deriving show { with_path = false }]

type method_modifier =
  | MAccess of access_modifier
  | MStatic (** UNSOPPORTED: only with main *)
[@@deriving show { with_path = false }]

type fild_modifier = FAccess of access_modifier [@@deriving show { with_path = false }]

type bin_op =
  | Asterisk (** [*] *)
  | Plus (** [+] *)
  | Minus (** [-] *)
  | Division (** [/] *)
  | Mod (** [%] *)
  | Equal (** [==] *)
  | NotEqual (** [!=] *)
  | Less (** [<] *)
  | LessOrEqual (** [<=] *)
  | More (** [>] *)
  | MoreOrEqual (** [>=] *)
  | And (** [&&] *)
  | Or (** [||] *)
  | Assign (** [=] *)
[@@deriving show { with_path = false }]

type un_op =
  | UMinus (** [-] *)
  | UNot (** [!] *)
  | New (** [new] *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of value_ (** assignable values *)
  (*  *)
  | EIdentifier of ident (** id of something e.g. class name; var name; method name *)
  | EMethod_invoke of expr * args (** [method(a, b, c)] or [Class.method(a, b, c)] *)
  | EPoint_access of expr * expr (** access by point e.g. [A.run()] *)
  (*  *)
  | EBin_op of bin_op * expr * expr
  | EUn_op of un_op * expr
(*  *)

and args = Args of expr list [@@deriving show { with_path = false }]

type var_decl = Var_decl of var_type * ident [@@deriving show { with_path = false }]
type params = Params of var_decl list [@@deriving show { with_path = false }]

type catch_decl =
  | CDecl of var_decl (** catch condition with declaration e.g. [catch(Exception a)] *)
  | CExn_id of ident (** catch condition with exception name [catch(Exception)] *)
[@@deriving show { with_path = false }]

type statement =
  | SExpr of expr
  | SDecl of var_decl * expr option
  | Steps of statement list
  | SReturn of expr option
  | SIf_else of expr * statement * statement option
  (** if-condition * if-body * else-body *)
  | SBreak
  | SThrow of expr
  | SWhile of expr * statement
  | SFor of
      { f_init_p : statement option
      ; f_cond_p : expr option
      ; f_iter_p : expr option
      ; f_body : statement
      }
  | STry_catch_fin of
      { try_s : statement
      ; catch_s : ((catch_decl * expr option) option * statement) list option
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
  ; m_params : params
  }
[@@deriving show { with_path = false }]

type constructor_sign =
  { con_modif : access_modifier option
  ; con_id : ident
  ; con_params : params
  ; base_args : args option
  }
[@@deriving show { with_path = false }]

type main_sign = meth_type [@@deriving show { with_path = false }]

type class_member =
  | Fild of fild_sign * expr option
  | Method of method_sign * statement
  | Constructor of constructor_sign * statement
[@@deriving show { with_path = false }]

type class_decl =
  { cl_modif : access_modifier option
  ; cl_id : ident
  ; parent : ident option
  ; cl_mems : class_member list
  }
[@@deriving show { with_path = false }]

type tast = Ast of class_decl list [@@deriving show { with_path = false }]
