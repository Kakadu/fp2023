open Types

(* --- TYPES FOR TYPECHECK & INTERPRETATION --- *)
type column =
  | CNumeric of int
  | CString of string
  | CReal of float
  | CBool of bool
[@@deriving show { with_path = false }]

type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
[@@deriving show { with_path = false }]

type expr = 
  | Const of value
  | Col of column
  | Plus of expr * expr
  | Minus of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr    (* = *)
  | NEqual of expr * expr   (* != *)
  | GThan of expr * expr    (* > *)
  | LThan of expr * expr    (* < *)
  | GThanEq of expr * expr  (* >= *)
  | LThanEq of expr * expr  (* <= *)
[@@deriving show { with_path = false }]

(* --- TYPECHECK --- *)

let check_column name = 

let transform_value = function
(* column *)
| Ast.Name x -> VString x (* TODO *)
(* values *)
| Ast.String x -> VString x
| Ast.Digit x -> VInt x
| Ast.Float_Digit x -> VFloat x
| Ast.Bool x -> VBool x

let transform_binary_operation e1 e2 = function
  | Ast.Add -> Plus (e1, e2)
  | Ast.Substract -> Minus (e1, e2)
  | Ast.Multiply -> Mul (e1, e2)
  | Ast.Divide -> Div (e1, e2)
  | Ast.Modulo -> Mod (e1, e2)
  | Ast.And -> And (e1, e2)
  | Ast.Or -> Or (e1, e2)
  | Ast.Equal -> Equal (e1, e2) 
  | Ast.Not_Equal -> NEqual (e1, e2)
  | Ast.Greater_Than -> GThan (e1, e2)
  | Ast.Less_Than -> LThan (e1, e2)
  | Ast.Less_Than_Or_Equal -> LThanEq (e1, e2)
  | Ast.Greater_Than_Or_Equal -> GThanEq (e1, e2)

module Eval (M : Utils.MONAD_FAIL) = struct
  open M

  (* let eval ... *)
end
