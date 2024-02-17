type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type const =
  | Int of int
  | Bool of bool
  | Empty

val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type pattern =
  | PAny
  | PConst of const
  | PVar of id

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Les
  | Leq
  | Gre
  | Geq

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

type expression =
  | Var of id
  | Const of const
  | IfThenElse of expression * expression * expression
  | Binop of bin_op * expression * expression
  | Fun of id * expression
  | List of expression list
  | Match of expression * (pattern * expression) list
  | App of expression * expression
  | Let of fun_rec * id * expression * expression option

and fun_rec =
  | Rec
  | NoRec

val pp_expression : Format.formatter -> expression -> unit
val pp_fun_rec : Format.formatter -> fun_rec -> unit
val show_expression : expression -> string
val show_fun_rec : fun_rec -> string

type expr_list = expression list

val pp_expr_list : Format.formatter -> expr_list -> unit
val show_expr_list : expr_list -> string
