type name = string [@@deriving show { with_path = false }]

type value =
  | Name of name
  | String of string
  | Digit of int
  | Float_Digit of float
  | Bool of bool
[@@deriving show { with_path = false }]

type bin_op =
  (* Arithmetic *)
  | Add
  | Substract
  | Multiply
  | Divide
  | Modulo
  (* Logic *)
  | And
  | Or
  (* Compare *)
  | Equal
  | Not_Equal
  | Greater_Than
  | Less_Than
  | Less_Than_Or_Equal
  | Greater_Than_Or_Equal
[@@deriving show { with_path = false }]

type unary_op = Not [@@deriving show { with_path = false }]

type expr =
  | Const of value
  | Unary_operation of unary_op * expr
  | Binary_operation of bin_op * expr * expr
[@@deriving show { with_path = false }]

type select_statement =
  | Asterisk
  | Expression of expr
[@@deriving show { with_path = false }]

type join_type =
  | Inner
  | Left
  | Right
  | Full
[@@deriving show { with_path = false }]

type from_statement =
  | Table of name
  | Join of 
  {   jtype : join_type;
      left  : from_statement;
      table : name;
      on    : expr 
  }
[@@deriving show { with_path = false }]

type request =
  { select : select_statement list
  ; from : from_statement
  ; where : expr option
  }
[@@deriving show { with_path = false }]
