(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | Int of int (** Int type *)
  | String of string (** String type *)
  | List of value list (** List type *)
  | Bool of bool (** Bool type *)
  | None (** None type *)

val equal_value : value -> value -> bool
val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

type arith_op =
  | Add (** Integer arithmetic addition *)
  | Sub (** Integer arithmetic subtraction *)
  | Mul (** Integer arithmetic multiplication *)
  | Div (** Integer arithmetic divison *)
  | Mod (** Integer arithmetic modulus *)

val equal_arith_op : arith_op -> arith_op -> bool
val pp_arith_op : Format.formatter -> arith_op -> unit
val show_arith_op : arith_op -> string

type identifier = Identifier of string (** A function or variable identifier *)

val equal_identifier : identifier -> identifier -> bool
val pp_identifier : Format.formatter -> identifier -> unit
val show_identifier : identifier -> string

type modifier =
  | Global (** Variable in a global scope *)
  | Class (** Variable in a class scope *)

val equal_modifier : modifier -> modifier -> bool
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

val equal_bool_op : bool_op -> bool_op -> bool
val pp_bool_op : Format.formatter -> bool_op -> unit
val show_bool_op : bool_op -> string

type f_string_type =
  | Str of value (** string in a f string *)
  | Var of identifier (** variable in a f string *)

type expression =
  | Const of value [@ocaml.doc " A constant that holds value "]
  | Variable of modifier * identifier
  [@ocaml.doc " Variable with a scope and its identifier "]
  | ArithOp of arith_op * expression * expression
  [@ocaml.doc " Arithmetic operation that consists of an operator and operands "]
  | BoolOp of bool_op * expression * expression
  [@ocaml.doc " Logical operation that consists of an operator and operands "]
  | FunctionCall of identifier * expression list
  [@ocaml.doc " A function call with its arguments "]
  | ListExp of expression list [@ocaml.doc " A list expression "]
  | Field of identifier * identifier [@ocaml.doc " A class field x.field "]
  | MethodCall of identifier * identifier * expression list
  [@ocaml.doc " A method call class.method() "]
  | Lambda of identifier list * expression [@ocaml.doc " Anonymous function "]
  | Object of identifier * expression list [@ocaml.doc " Instance of a class "]
  | FString of f_string_type list [@ocaml.doc " F string "]

val equal_f_string_type : f_string_type -> f_string_type -> bool
val equal_expression : expression -> expression -> bool
val pp_f_string_type : Format.formatter -> f_string_type -> unit
val show_f_string_type : f_string_type -> string
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
  | For of expression * expression list * statement list
  (** Else statemtn with a guards and its body *)
  | Class of identifier * statement list (** Class with its identifier and contents *)
  | Return of expression (** Return statement *)
  | Error (** Error *)

val equal_statement : statement -> statement -> bool
val pp_statement : Format.formatter -> statement -> unit
val show_statement : statement -> string

type flag =
  | No (** No flag *)
  | Return_f (** Return flag *)
