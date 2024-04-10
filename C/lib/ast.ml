(** Copyright 2023-2024, PavlushaSource *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show {with_path= false}]

type types =
  | ID_bool
  | ID_int32
  | ID_int16
  | ID_int8
  | ID_uint32
  | ID_uint16
  | ID_uint8
  | ID_char
  | ID_void
  | ID_float
  | Pointer of types
  | Array of int option * types
[@@deriving show {with_path= false}]

type arg = Arg of types * name [@@deriving show {with_path= false}]

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Grow
  | GrowOrEqual
  | Or
  | And
  | Lshift
  | Rshift
[@@deriving show {with_path= false}]

type un_op =
  | Not  (** !a *)
  | Plus  (** +a *)
  | Minus  (** -(--a) *)
  | Address  (** &a *)
  | Dereference  (** *a *)
  | Pref_increment  (** ++a *)
  | Pref_decrement  (** --a *)
[@@deriving show {with_path= false}]

type values =
  | V_int of int
  | V_char of char
  | V_float of float
  | V_void
  | V_null
[@@deriving show {with_path= false}]

type expr =
  | Unary_expr of un_op * expr
  | Bin_expr of bin_op * expr * expr
  | Const of values
  | Type of types  (** sizeof(int) *)
  | Func_call of name * expr list  (** factorial(1, 2 + 3, 5 > 7) *)
  | Var_name of name  (** return <var_name>*)
  | Cast of types * expr  (** (int) a *)
  | Index of expr * expr  (** a[1][2] => Index(Index(Var_name(a), 1), 2) *)
  | Array_value of expr list
[@@deriving show {with_path= false}]

type statement =
  | Var_decl of types * name * statement option  (** int **b[1][2]; *)
  | Assign of expr * statement  (** int n = b = 4*)
  | Expression of expr
  | Return of expr
  | Compound of statement list  (** { n = 4; { n = 3;...} here n = 4 } *)
  | While of expr * statement
  | For of statement option * expr option * expr option * statement
    (** for (init?; cond?; upd?) { expr list } *)
  | If_else of expr * statement * statement option
  | Break
  | Continue
[@@deriving show {with_path= false}]

type func_decl = Func_decl of types * name * arg list * statement
[@@deriving show {with_path= false}]

type program = func_decl list [@@deriving show {with_path= false}]
