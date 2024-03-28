(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | VInt of int
  | VChar of char
  | VBool of bool
  | VString of string
[@@deriving show { with_path = false }, eq]

type name = Name of string [@@deriving show { with_path = false }, eq]

type typ =
  | TInt
  | TChar
  | TBool
  | TString
  | TObj of name
[@@deriving show { with_path = false }, eq]

type var_type = TVar of typ [@@deriving show { with_path = false }, eq]

type meth_type =
  | TVoid (** type [void] for methods *)
  | TReturn of typ
[@@deriving show { with_path = false }, eq]

type access_modifier =
  | Public
  | Protected
  | Private
[@@deriving show { with_path = false }, eq]

type poly_modifier =
  | Static
  | Override
  | Virtual
  | MNew
[@@deriving show { with_path = false }, eq]

type bin_op =
  | Plus (** + *)
  | Asterisk (** * *)
  | Dash (** - *)
  | Slash (** / *)
  | Mod (** % *)
  | Equal (** == *)
  | NotEqual (** != *)
  | Less (** < *)
  | LessOrEqual (** <= *)
  | More (** > *)
  | MoreOrEqual (** >= *)
  | And (** && *)
  | Or (** || *)
  | Assign (** = *)
[@@deriving show { with_path = false }, eq]

type un_op =
  | Not (** ! *)
  | Minus (** - *)
  | New (** new *)
[@@deriving show { with_path = false }, eq]

type args = Args of expr list

and expr =
  | Exp_Const of value
  | Bin_op of bin_op * expr * expr
  | Un_op of un_op * expr
  | Access_By_Point of expr * expr (**  Access by point to class member [a.b.c]*)
  | Exp_Name of name
  | Method_invoke of expr * args (** Calling a method [Foo.a(i, 9)] *)
[@@deriving show { with_path = false }, eq]

type var_declaration =
  | Var_Declaration of var_type * name (** Variable declaration [int a] *)
[@@deriving show { with_path = false }, eq]

type params = Params of var_declaration list [@@deriving show { with_path = false }, eq]

type statement =
  | Expr of expr
  | Decl of var_declaration * expr option
  (** Declare and assign a value to a variable [int a = 1]*)
  | If of expr * statement * statement option
  | For of statement option * expr option * expr option * statement
  | While of expr * statement
  | Return of expr option
  | Body of statement list (* add Instance constructor invocation -- new T(x, y)*)
[@@deriving show { with_path = false }, eq]

type field =
  { f_modifier : access_modifier option
  ; f_type : var_type
  ; f_name : name
  }
[@@deriving show { with_path = false }, eq]

type methods =
  { m_acc_modifier : access_modifier option
  ; m_poly_modifier : poly_modifier option
  ; m_type : meth_type
  ; m_name : name
  ; m_params : params
  }
[@@deriving show { with_path = false }, eq]

type constructor =
  { c_modifier : access_modifier option
  ; c_name : name
  ; c_params : params
  ; c_base : args option
  }
[@@deriving show { with_path = false }, eq]

type member =
  | CField of field * expr option
  | CMethod of methods * statement
  | CConstructor of constructor * statement
[@@deriving show { with_path = false }, eq]

type i_member = IMethod of methods [@@deriving show { with_path = false }, eq]

type obj_class =
  { cl_modifier : access_modifier option
  ; cl_name : name
  ; cl_parent : name option
  ; cl_body : member list
  }
[@@deriving show { with_path = false }, eq]

type interface =
  { i_modifier : access_modifier option
  ; i_name : name
  ; i_parent : name option
  ; i_body : i_member list
  }
[@@deriving show { with_path = false }, eq]

type objects =
  | Class of obj_class
  | Interface of interface
[@@deriving show { with_path = false }, eq]

type program = Ast of objects list [@@deriving show { with_path = false }, eq]
