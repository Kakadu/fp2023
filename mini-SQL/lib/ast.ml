type value =
  | Name of string
  | String of string
  | Digit of int
  | Float_Digit of float
  | Bool of bool
[@@deriving show { with_path = false }]

type unary_op = Not [@@deriving show { with_path = false }]

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

type expr =
  | Const of value
  | Unary_operation of unary_op * expr
  | Binary_operation of bin_op * expr * expr
[@@deriving show { with_path = false }]

type select_expr =
  | All_Columns
  | Expr of expr
[@@deriving show { with_path = false }]

type command =
  | Select of
      { exprs : select_expr list
      ; table : string
      ; condition : expr option
      }
[@@deriving show { with_path = false }]
