type id = string 

type const =
  | Bool of bool
  | Int of int
  | String of string
  | Float of float

type binop = 
  | Plus	(*  +   *)
  | Minus	(*  -   *)
  | Mult	(*  *   *)
  | Div	(*  /   *)
  | Mod	(*  mod *)
  | And	(*  &&  *)
  | Or	(*  ||  *)
  | Eq	(*  ==  *)
  | Neq	(*  <>  *)
  | Lt	(*  <   *)
  | Ltq	(*  <=  *)
  | Gt	(*  >   *)
  | Gtq	(*  >=  *)

type unop =
  | Not (** not a *)
  | Minus (*  -5  *)

type pattern = 
  | PVar of id
  | PConst of const (** fun n -> match n with 0 -> 0 | n -> 1 *)
  | PCons of pattern * pattern (** PCons (p1,p2) is p1::p2    *)
  | PAny (* _ *)

type expr = 
  | EVar of id
  | EConst of const
  | BinOp of binop * expr * expr (** EBinOp (op,l,r) is l op r *)
  | UnOp of unop * expr (** EUnOp (op,e) is op e *)
  | Fun of id * expr (** Fun x -> e *)
  | ECons of expr * expr (** ECons (h,t) is list h::t *)
  | Tuple of expr list (** (expr1, ..., exprn) *)
  | IfThenElse of expr * expr * expr (** IfThenElse (b,t,e) is if b then t else e *)
  | Let of id * expr * expr (** Let (x,e,e') is let x = e in e' *)
  | LetRec of id * expr * expr (** LetRec ("x",e,e') is let rec x e in e' *)
  | App of expr * expr (**  App (f,e) is application f e *)
  | Match of expr * (pattern * expr) 