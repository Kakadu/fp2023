(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** name var or fun *)
type name = string [@@deriving show { with_path = false }]

type const =
  | CBool of bool (** true *)
  | CInt of int (** 42 *)
  | CString of string (** "string" *)
  | CNil (** () [] *)
[@@deriving show { with_path = false }]

type binary_op =
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
  | Const of const (** string, bool or int *)
  | Var of name (** variable *)
  | Tuple of pattern list (** (a, b) *)
  | List of pattern list (** [1;2;3] *)
  | Case of name * pattern list (** in patmatch *)
[@@deriving show { with_path = false }]

type expr =
  | ConstExpr of const
  | VarExpr of name (** x = 5 *)
  | ListExpr of expr list (** [1;2;3]*)
  | TupleExpr of expr list (** (1,2,3)*)
  | BinExpr of binary_op * expr * expr (** 1 + 5 - 3*)
  | IfExpr of expr * expr * expr (** if a then b else c *)
  | LetExpr of bool * name * expr (** let sq x = x * x, bool - is rec or not*)
  | LetActExpr of name list * expr
  (** let (|Even|Odd|) ..., let (|Even|_|) ..., let (|Even|)... *)
  | AppExpr of expr * expr (** sq 5 *)
  | FunExpr of pattern * expr (** fun x -> x * x *)
  | MatchExpr of expr * (pattern * expr) list (** match input with | 2 -> 5 | 5 -> 10 *)
  | CaseExpr of name * expr list (** (|Even|Odd|), Some x *)
[@@deriving show { with_path = false }]

(** main program *)
type program = expr list [@@deriving show { with_path = false }]

type value =
  | VString of string (** "string" *)
  | VBool of bool (** true *)
  | VInt of int (** 42 *)
  | VList of value list (** [1;2;3] *)
  | VTuple of value list (** (1,2,3) *)
  | VFun of pattern * expr * (name * value) list
  | VLetWAPat of name * value (** Let without active patterns, let x y = y * 5 *)
  | VLetAPat of name list * value
  (** Let with active patterns, let (|Even|Odd|) value = .., let (|Even|_|) value = .., let (|Even|) value = ..*)
  | VCases of name (** Even, Odd, ... *)
  | VNone
[@@deriving show { with_path = false }]
