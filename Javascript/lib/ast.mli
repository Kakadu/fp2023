type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
[@@derving show {with_path = false}]

type typename =
  | Number of float
  | VarType
  | FuncType
[@@derving show {with_path = false}]

type expression =
  | BinOp of bin_op * expression * expression
  | UnrecognizedOp of bin_op
  | Parens of expression
  | Const of typename
  | Var of string
  | FunctionCall of string * expression list
  | DebugExp of expression list
[@@derving show {with_path = false}]

type var_init =
{
  var_identifier: string;
  is_const: bool;
  var_type: typename;
  value: expression option;
}

and fun_init =
{
  fun_identifier: string;
  is_rec: bool;
  arguments: expression list;
  value_type: typename;
  body: statement;
}

and statement =
  | Block of statement list
  | Expression of expression
  | VarDeck of var_init
  | FunDeck of fun_init
  | If of expression * statement * statement option
  | Return of expression
  | DebugStm of string
  | Programm of statement list
[@@deriving show { with_path = false }]