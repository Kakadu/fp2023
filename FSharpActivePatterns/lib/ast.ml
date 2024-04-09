(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** name var or fun *)
type name = string [@@deriving eq, show { with_path = false }]

type const =
  | CBool of bool (** true *)
  | CInt of int (** 42 *)
  | CString of string (** "string" *)
  | CNil (** [] *)
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Add (** 1 + 1 *)
  | Sub (** 2 - 1 *)
  | Mul (** 5 * 4 *)
  | Div (** 8 / 2 *)
  | Mod (** 10 % 5 *)
  | Less (** 7 < 10 *)
  | LEq (** 10 <= 10 *)
  | Gre (** 7 > 5 *)
  | GEq (** 7 >= 7 *)
  | Eq (** 5 = 5 *)
  | NEq (** 4 <> 3 *)
  | And (** true && true *)
  | Or (** true || false *)
  | Con (** hd::tl *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | Wild (** _ *)
  | Const of const (** string, bool or int *)
  | Var of name (** variable *)
  | Tuple of pattern list (** (a, b) *)
  | PCon of pattern * pattern (** hd::tl *)
  | Case of name * pattern list (** in patmatch *)
[@@deriving eq, show { with_path = false }]

type expr =
  | ConstExpr of const
  | VarExpr of name (** x = 5 *)
  | ListExpr of expr * expr (** [1;2;3]*)
  | TupleExpr of expr list (** (1,2,3)*)
  | BinExpr of binary_op * expr * expr (** 1 + 5 - 3*)
  | IfExpr of expr * expr * expr (** if a then b else c *)
  | LetExpr of bool * name * expr (** let sq x = x * x, bool - is rec or not*)
  | LetInExpr of bool * name * expr * expr
  (** let plusfive x = let five a = a + 5 in five x*)
  | LetActExpr of name list * expr
  (** let (|Even|Odd|) value =  if value % 2 = 0 then Even else Odd *)
  | AppExpr of expr * expr (** sq 5 *)
  | FunExpr of pattern * expr (** fun x -> x * x *)
  | MatchExpr of expr * (pattern * expr) list (** match input with | 2 -> 5 | 5 -> 10 *)
  | CaseExpr of name (** (|Even|Odd|) *)
[@@deriving eq, show { with_path = false }]
