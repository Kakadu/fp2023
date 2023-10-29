type name = string [@@deriving eq, show { with_path = false }]

type const =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Plus
  | Dash
  | Asterisk
  | Slash
  | Eq
  | And
  | Or
  | Neq
  | Less
  | Lessq
  | Greater
  | Greaterq
  | Cons
[@@deriving eq, show { with_path = false }]

type decl_type =
  | IntType
  | StringType
  | UndefinedType
  | BoolType
  | CharType
  | UnitType
  | TupleType of decl_type list
  | ListType of decl_type
  | FuncType of decl_type * decl_type
[@@deriving eq, show { with_path = false }]

type arg =
  | NoLabel of name * decl_type
  | Label of name * decl_type
  | Optional of name * decl_type
[@@deriving eq, show { with_path = false }]

type expr =
  | Const of const
  | BinOp of bin_op * expr * expr
  | Var of name
  | Apply of expr * expr
  | Fun of arg list * decl_type * expr
  | IfThenElse of expr * expr * expr
  | EDecl of bool * name * arg list * decl_type * expr * expr
[@@deriving eq, show { with_path = false }]

type decl = LetDecl of bool * name * arg list * decl_type * expr
[@@deriving eq, show { with_path = false }]

type program = decl list [@@deriving eq, show { with_path = false }]

let let_decl a b c d e = LetDecl (a, b, c, d, e)
let let_edecl a b c d e f = EDecl (a, b, c, d, e, f)
