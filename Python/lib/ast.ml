(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*Standart data types: integers, strings, lists*)
type value =
  (*Int type*)
  | Int of int
  (*String type*)
  | String of string
  (*List type*)
  | List of value list
  (*Bool type*)
  | Bool of bool
  (*None type*)
  | None
[@@deriving eq, show { with_path = false }]

(*Standart arithmetic operations *)
type arith_op =
  (*Integer arithmetic addition*)
  | Add
  (*Integer arithmetic subtraction*)
  | Sub
  (*Integer arithmetic multiplication*)
  | Mul
  (*Integer arithmetic divison*)
  | Div
  (*Integer arithmetic modulus*)
  | Mod
[@@deriving eq, show { with_path = false }]

(*Funcions' name & args' name*)
type identifier = Identifier of string [@@deriving eq, show { with_path = false }]

type modifier =
  (*Variable in a global scope*)
  | Global
  (*Variable in a class scope*)
  | Class
[@@deriving eq, show { with_path = false }]

(*Standart boolean operators*)
type bool_op =
  (*logical and*)
  | And
  (*logical or*)
  | Or
  (*logical equals*)
  | Equal
  (*logical not equals*)
  | NotEqual
  (*logical greater or equals*)
  | GreaterOrEqual
  (*logical greater*)
  | Greater
  (*logical less or equals*)
  | LessOrEqual
  (*logical less*)
  | Less
[@@deriving eq, show { with_path = false }]

type f_string_type =
  (*string in a f string*)
  | Str of value
  (*variable in a f string*)
  | Var of identifier
[@@deriving eq, show { with_path = false }]

(*string or variable in a f string*)
type f_string_elem = FStringElem of f_string_type
[@@deriving eq, show { with_path = false }]

(*Standart expressions*)
and expression =
  (*A constant that holds value*)
  | Const of value
  (*Variable with a scope and its identifier*)
  | Variable of modifier * identifier
  (*Arithmetic operation that consists of an operator and operands*)
  | ArithOp of arith_op * expression * expression
  (*Logical operation that consists of an operator and operands*)
  | BoolOp of bool_op * expression * expression
  (*A function call with its arguments*)
  | FunctionCall of identifier * expression list
  (*A list expression*)
  | List of expression list
  (*A class field x.field*)
  | Field of identifier * identifier
  (*A method call class.method()*)
  | MethodCall of identifier * identifier * expression list
  (*Anonymous function*)
  | Lambda of identifier list * expression
  (*Instance of a class*)
  | Object of identifier * expression list
  (*F string*)
  | FString of f_string_elem list
[@@deriving eq, show { with_path = false }]

(*Standart statements*)
type statement =
  (*Statement which is expression*)
  | Expression of expression
  (*Assign statement*)
  | Assign of expression * expression
  (*A function declartion with its identifier and body*)
  | Function of identifier * identifier list * statement list
  (*If else statemtn with a guard both if and else body*)
  | IfElse of expression * statement list * statement list
  (*Else statemtn with its body*)
  | Else of statement list
  (*Else statemtn with a guard and its body*)
  | While of expression * statement list
  (*Else statemtn with a guards and its body*)
  | For of expression * expression list * statement list
  (*Class with its identifier and contents*)
  | Class of identifier * statement list
  (*Return statement*)
  | Return of expression
[@@deriving eq, show { with_path = false }]

type flag =
  | No
  | Return_f
