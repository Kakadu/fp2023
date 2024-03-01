(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | Int of int (** Int type *)
  | String of string (** String type *)
  | List of value list (** List type *)
  | Bool of bool (** Bool type *)
  | Nil (** None type *)

val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

type arith_op =
  | Add (** Integer arithmetic addition *)
  | Sub (** Integer arithmetic subtraction *)
  | Mul (** Integer arithmetic multiplication *)
  | Div (** Integer arithmetic divison *)
  | Mod (** Integer arithmetic modulus *)

val pp_arith_op : Format.formatter -> arith_op -> unit
val show_arith_op : arith_op -> string

type identifier = Identifier of string (** A function or variable identifier *)

val pp_identifier : Format.formatter -> identifier -> unit
val show_identifier : identifier -> string

type modifier =
  | Global (** Variable in a global scope *)
  | Class (** Variable in a class scope *)

val pp_modifier : Format.formatter -> modifier -> unit
val show_modifier : modifier -> string

type bool_op =
  | And (** logical and *)
  | Or (** logical or *)
  | Equal (** logical equals *)
  | NotEqual (** logical not equals *)
  | GreaterOrEqual (** logical greater or equals *)
  | Greater (** logical greater *)
  | LessOrEqual (** logical less or equals *)
  | Less (** logical less *)

val pp_bool_op : Format.formatter -> bool_op -> unit
val show_bool_op : bool_op -> string

type f_string_type =
  | Str of value (** string in a f string *)
  | Var of identifier (** variable in a f string *)

val pp_f_string_type : Format.formatter -> f_string_type -> unit
val show_f_string_type : f_string_type -> string

type expression =
  | Const of value (** A constant that holds value *)
  | Variable of modifier * identifier (** Variable with a scope and its identifier *)
  | ArithOp of arith_op * expression * expression
  (** Arithmetic operation that consists of an operator and operands *)
  | BoolOp of bool_op * expression * expression
  (** Logical operation that consists of an operator and operands *)
  | FunctionCall of identifier * expression list (** A function call with its arguments *)
  | ListExp of expression list (** A list expression *)
  | Field of identifier * identifier (** A class field x.field *)
  | MethodCall of identifier * identifier * expression list
  (** A method call class.method() *)
  | Lambda of identifier list * expression (** Anonymous function *)
  | FString of f_string_type list (** F string *)

val pp_expression : Format.formatter -> expression -> unit
val show_expression : expression -> string

type statement =
  | Expression of expression (** Statement which is expression *)
  | Assign of expression * expression (** Assign statement *)
  | Function of identifier * identifier list * statement list
  (** A function declartion with its identifier and body *)
  | IfElse of expression * statement list * statement list
  (** If else statemtn with a guard both if and else body *)
  | Else of statement list (** Else statemtn with its body *)
  | While of expression * statement list (** Else statemtn with a guard and its body *)
  | Class of identifier * statement list (** Class with its identifier and contents *)
  | Return of expression (** Return statement *)
  | Setattr of identifier * expression * identifier (** Dynamic method addition *)

val pp_statement : Format.formatter -> statement -> unit
val show_statement : statement -> string

type flag =
  | No (** No flag *)
  | Return_f (** Return flag *)
