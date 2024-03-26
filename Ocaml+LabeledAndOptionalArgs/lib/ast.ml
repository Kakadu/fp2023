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
  | EmptyType (*If the type is not explicitly specified*)
  | BoolType of is_option
  | CharType of is_option
  | UnitType of is_option
  | TupleType of decl_type list * is_option
  | ListType of decl_type * is_option
  | FuncType of decl_type * decl_type * is_option
[@@deriving eq, show { with_path = false }]

type label =
  | Label of name option
  | Optional of expr option
[@@deriving eq, show { with_path = false }]

and pattern =
  | PNill (* [] *)
  | PEmpty (* _ *)
  | PArg of name * label
  | PConst of const
  | PVar of name
  | PCons of pattern * pattern
  | PTuple of pattern list
[@@deriving eq, show { with_path = false }]

and decl = LetDecl of is_rec * name * expr [@@deriving eq, show { with_path = false }]

and expr =
  | Const of const
  | BinOp of bin_op * expr * expr
  | Var of name
  | LabeledVar of name * expr
  | Apply of expr * expr
  | Fun of pattern * expr
  | IfThenElse of expr * expr * expr
  | EDecl of decl * expr
  | Match of expr * (pattern * expr) list
[@@deriving eq, show { with_path = false }]

type program = decl list [@@deriving eq, show { with_path = false }]

let ematch e pl = Match (e, pl)
let efun p e = Fun (p, e)
let let_decl b s e = LetDecl (b, s, e)
let edecl d e = EDecl (d, e)
