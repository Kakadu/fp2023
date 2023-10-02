type name = string

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t

(*Standart data types: integers, strings, lists*)
type value = 
  | Int of int
  | String of string
  | List of value list

(*Standart arithmetic operations *)
  type arith_op =
  | Add
  | Sub
  | Mul
  | Div 

(*Standart boolean operators*)
  type bool_op = 
  | And
  | Or

(*Standart expressions*)
type expression = 
  | Const of value
  | Var of string
  | ArithOp of arith_op * expression * expression
  | BoolOp of bool_op * expression * expression

(*Standart statements*)
type statement = 
  | Expression of expression
  | If of expression * expression * expression
  | While of expression * statement list
  (*todo: for*)
