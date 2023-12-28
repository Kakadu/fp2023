(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*Standart data types: integers, strings, lists*)
type value =
  | Int of int (** Int type **)
  | String of string (** String type **)
  | List of value list (** List type **)
  | Bool of bool (** Bool type **)
  | None (** None type **)
[@@deriving eq, show { with_path = false }]

(*Standart arithmetic operations *)
type arith_op =
  | Add (** Integer arithmetic addition **)
  | Sub (** Integer arithmetic subtraction **)
  | Mul (** Integer arithmetic multiplication **)
  | Div (** Integer arithmetic divison **)
  | Mod (** Integer arithmetic modulus **)
[@@deriving eq, show { with_path = false }]

(*Funcions' name & args' name*)
type identifier = Identifier of string [@@deriving eq, show { with_path = false }]

type modifier =
  | Global (** Variable in a global scope **)
  | Class (** Variable in a class scope **)
[@@deriving eq, show { with_path = false }]

(*Standart boolean operators*)
type bool_op =
  | And (** logical and **)
  | Or (** logical or **)
  | Equal (** logical equals **)
  | NotEqual (** logical not equals **)
  | GreaterOrEqual (** logical greater or equals **)
  | Greater (** logical greater **)
  | LessOrEqual (** logical less or equals **)
  | Less (** logical less **)
[@@deriving eq, show { with_path = false }]

type f_string_type =
  | Str of value (** string in a f string **)
  | Var of identifier (** variable in a f string **)
[@@deriving eq, show { with_path = false }]

(*string or variable in a f string*)
type f_string_elem = FStringElem of f_string_type
[@@deriving eq, show { with_path = false }]

(*Standart expressions*)
and expression =
  | Const of value (** A constant that holds value **)
  | Variable of modifier * identifier (** Variable with a scope and its identifier **)
  | ArithOp of arith_op * expression * expression
  (** Arithmetic operation that consists of an operator and operands **)
  | BoolOp of bool_op * expression * expression
  (** Logical operation that consists of an operator and operands**)
  | FunctionCall of identifier * expression list
  (** A function call with its arguments **)
  | List of expression list (** A list expression **)
  | Field of identifier * identifier (** A class field x.field **)
  | MethodCall of identifier * identifier * expression list
  (** A method call class.method() **)
  | Lambda of identifier list * expression (** Anonymous function **)
  | Object of identifier * expression list (** Instance of a class **)
  | FString of f_string_elem list (** F string **)
[@@deriving eq, show { with_path = false }]

(*Standart statements*)
type statement =
  | Expression of expression (** Statement which is expression **)
  | Assign of expression * expression (** Assign statement **)
  | Function of identifier * identifier list * statement list
  (** A function declartion with its identifier and body **)
  | IfElse of expression * statement list * statement list
  (** If else statemtn with a guard both if and else body **)
  | Else of statement list (** Else statemtn with its body **)
  | While of expression * statement list (** Else statemtn with a guard and its body **)
  | For of expression * expression list * statement list
  (** Else statemtn with a guards and its body **)
  | Class of identifier * statement list (** Class with its identifier and contents **)
  | Return of expression (** Return statement **)
[@@deriving eq, show { with_path = false }]

type flag =
  | No (** no flag **)
  | Return_f (** return flag **)
