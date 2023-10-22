type const =
  | Int of int
  | Bool of bool
[@@deriving eq, show { with_path = false }]

type un_op =
  | Un_plus
  | Un_minus
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Leq
[@@deriving eq, show { with_path = false }]

type val_name = LCIdent of string (** variable_name1 *)
[@@deriving eq, show { with_path = false }]

(* type pattern =
   | Any
   | Pat_val of val_name
   | Pat_const of const
   | Unit
   | Ntuple of pattern list
   | Par_Cons *)

type expr =
  | Expr_const of const
  | Un_op of un_op * expr
  | Bin_op of bin_op * expr * expr
  | Expr_val of val_name
  | ITE of expr * expr * expr
  | Fun of val_name * expr
  | App of expr * expr
  | Expr_let of decl * expr
[@@deriving eq, show { with_path = false }]

and decl = bool * val_name * expr
and let_decl = Let_decl of decl

type program = let_decl list [@@deriving eq, show { with_path = false }]
