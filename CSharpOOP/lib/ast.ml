(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | VInt of int
  | VChar of char
  | VBool of bool
  | VString of string
  | NULL (** Null value*)
[@@deriving show { with_path = false }, eq]

type ref_type =
  | TString
  | TClass
  | TInterface
[@@deriving show { with_path = false }, eq]

type base_type =
  | TInt
  | TChar
  | TBool
[@@deriving show { with_path = false }, eq]

type not_nullable_type =
  | TBase of base_type
  | TRef of ref_type
[@@deriving show { with_path = false }, eq]

type type_ =
  | TNot_nullable_type of not_nullable_type
  | TNullable_type of base_type
[@@deriving show { with_path = false }, eq]

type var_type = TVar of type_ [@@deriving show { with_path = false }, eq]

type meth_type =
  | TVoid (** type [void] for methods *)
  | TRetrun of type_
[@@deriving show { with_path = false }, eq]

type name = Name of string [@@deriving show { with_path = false }, eq]

type access_modifier =
  | Public
  | Protected
  | Private
[@@deriving show { with_path = false }, eq]

type field_modifier =
  | New
  | FAccess of access_modifier
[@@deriving show { with_path = false }, eq]

type class_modifier = CAccess of access_modifier
[@@deriving show { with_path = false }, eq]

type method_modifier =
  | MAccess of access_modifier
  | Static
  | Override
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
[@@deriving show { with_path = false }, eq]

type expr =
  | Exp_Const of value
  | Bin_op of bin_op * expr * expr
  | Un_op of un_op * expr
  | Access_By_Point of expr * expr (**  Access by point to class member [a.b.c]*)
  | Exp_Name of name
  | Exp_Args of expr list
  | Method_invoke of expr * expr (** Calling a method [Foo.a(i, 9)] *)
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
  | Return of expr option
  | Body of statement list
[@@deriving show { with_path = false }, eq]

type field = Field of field_modifier list option * var_type * name
[@@deriving show { with_path = false }, eq]

type methods = Method of method_modifier list option * meth_type * name * params
[@@deriving show { with_path = false }, eq]

type field_sign = Field_Sign of field * expr option
[@@deriving show { with_path = false }, eq]

type method_sign = Method_Sign of methods * statement
[@@deriving show { with_path = false }, eq]

type member =
  | CField of field_sign
  | CMethod of method_sign
[@@deriving show { with_path = false }, eq]

type i_member =
  | IField of field
  | IMethod of methods
[@@deriving show { with_path = false }, eq]

type objects =
  | Class of class_modifier list option * name * member list
  | Interface of class_modifier option * name * i_member list
[@@deriving show { with_path = false }, eq]

type program = Ast of objects list option [@@deriving show { with_path = false }, eq]
