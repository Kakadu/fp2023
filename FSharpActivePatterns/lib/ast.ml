(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type const =
  | CBool of bool (** true *)
  | CInt of int (** 42 *)
  | CString of string (** "string" *)
  | CNil
[@@deriving show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | Less (** < *)
  | LEq (**  <= *)
  | Gre (** > *)
  | GEq (**  >= *)
  | Eq (** = *)
  | NEq (** <> *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type pattern =
  | Wild (** _ *)
  | Const of const
  | Var of name
  | Tuple of pattern list (** a, b *)
  | List of pattern list (** [1;2;3] *)
  | Case of name * pattern list
[@@deriving show { with_path = false }]

type pattern_type =
  | SingleChoice of name * bool
  | MultipleChoice of name list
[@@deriving show { with_path = false }]

type active_pattern =
  | Name of name
  | ActivePaterns of pattern_type
[@@deriving show { with_path = false }]

type expr =
  | ConstExpr of const
  | VarExpr of name
  | ListExpr of expr list
  | TupleExpr of expr list
  | BinExpr of bin_op * expr * expr (** 1 + 5 - 3*)
  | IfExpr of expr * expr * expr (** if a then b else c *)
  | LetExpr of bool * active_pattern * expr (** let sq x = x * x *)
  | AppExpr of expr * expr (** sq 5 *)
  | FunExpr of pattern * expr (** fun x -> x * x *)
  | MatchExpr of expr * (pattern * expr) list
  | CaseExpr of name * expr list
[@@deriving show { with_path = false }]

type program = expr list [@@deriving show { with_path = false }]

(** for interpret *)
type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * (name * value) list
  | VLetWAPat of name * value (* without active patterns *)
  | VLetAPat of pattern_type * value (* with active patterns *)
  | VCases of name * value option
  | VSome of value
  | VNone
  | VUnit
  | VNil
[@@deriving show { with_path = false }]
