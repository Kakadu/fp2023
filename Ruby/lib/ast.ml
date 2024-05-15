
type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Char of char
  | Null
[@@deriving show { with_path = false }, variants]

type id = Id of string (* Id(string) *)
[@@deriving show { with_path = false }, variants]

type expr =
  | Const of value
  | Var of id
  | Plus of expr * expr
  | Minus of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | LessOrEqual of expr * expr
  | Mult of expr * expr (* a * b *)
  | Div of expr * expr (* a \ b *)
  | FuncCall of id * expr list (* f() *)
  | List of expr list
[@@deriving show { with_path = false }, variants]

type statement =
  | Assign of expr * expr
  | Expr of expr (* *)
  | Func of id * id list * statement list (* id args stmt *)
  | IfElse of expr * statement list * statement list (* expr stmt(if) stmt(else) *)
  | Returns of expr
  | Continue
  | Break
  | Puts of expr
[@@deriving show { with_path = false }, variants]

type ast = statement list [@@deriving show { with_path = false }]