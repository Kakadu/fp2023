(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

type signal =
  | Work
  | Break
  | Next
  | Return

type id = Id of string (* Id(string) *) [@@deriving show { with_path = false }]

type modifier =
  | LocalVar
  | InstanceVar
  | GlobalVar
  | ClassVar
[@@deriving show { with_path = false }]

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Null
  | Object of id
  | Lambda of id list * statement list
  | ListExpr of expr list
[@@deriving show { with_path = false }]

and expr =
  | Const of value
  | Var of modifier * id
  | Plus of expr * expr
  | Minus of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | LessOrEqual of expr * expr
  | Less of expr * expr
  | GreaterOrEqual of expr * expr
  | Greater of expr * expr
  | Mult of expr * expr (* a * b *)
  | Div of expr * expr (* a / b *)
  | FuncMonoCall of id * expr list (* f() *)
  | FuncPolyCall of id * id * expr list (* x.f() *)
  | ListAccess of id * expr
  | ModOp of expr * expr
  | AndOp of expr * expr
  | OrOp of expr * expr
  | CallLambda of id list * statement list * expr list
[@@deriving show { with_path = false }]

and statement =
  | Assign of expr * expr
  | MultiAssign of expr list * expr list (** x, y = 1, 2 *)
  | Expr of expr
  | Func of id * id list * statement list (* id args stmt *)
  | IfElse of expr * statement list * statement list (* expr stmt(if) stmt(else) *)
  | Return of expr
  | Continue
  | Break
  | Puts of expr
  | While of expr * statement list
  | Class of id * statement list
[@@deriving show { with_path = false }]

type ast = statement list [@@deriving show { with_path = false }]
