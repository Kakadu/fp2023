(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]
type is_option = bool [@@deriving eq, show { with_path = false }]
type is_rec = bool [@@deriving eq, show { with_path = false }]

type const =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Plus (* + *)
  | Dash (* - *)
  | Asterisk (* * *)
  | Slash (* / *)
  | Eq (* = *)
  | And (* && *)
  | Or (* || *)
  | Neq (* <> *)
  | Less (* < *)
  | Lessq (* <= *)
  | Greater (* > *)
  | Greaterq (* >= *)
  | Cons (* :: *)
[@@deriving eq, show { with_path = false }]

type decl_type =
  | IntType of is_option
  | StringType of is_option
  | UndefinedType (*If the type is not explicitly specified*)
  | BoolType of is_option
  | CharType of is_option
  | UnitType of is_option
  | TupleType of decl_type list * is_option
  | ListType of decl_type * is_option
  | FuncType of decl_type * decl_type * is_option
[@@deriving eq, show { with_path = false }]

type arg =
  | NoLabel of name * decl_type
  | Label of name * name * decl_type
  | Optional of name * const option * decl_type
[@@deriving eq, show { with_path = false }]

type expr =
  | Const of const
  | BinOp of bin_op * expr * expr
  | Var of name
  | Apply of expr * expr
  | Fun of arg list * decl_type * expr
  | IfThenElse of expr * expr * expr
  | EDecl of bool * name * arg list * decl_type * expr * expr
[@@deriving eq, show { with_path = false }]

type decl = LetDecl of is_rec * name * arg list * decl_type * expr
[@@deriving eq, show { with_path = false }]

type program = decl list [@@deriving eq, show { with_path = false }]

let let_decl a b c d e = LetDecl (a, b, c, d, e)
let let_edecl a b c d e f = EDecl (a, b, c, d, e, f)
