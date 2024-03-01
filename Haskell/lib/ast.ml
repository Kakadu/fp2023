(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Unique identifier typically representing variable names. *)
type id = string [@@deriving show { with_path = false }]

(* Type representing unary operations. *)
type un_op =
  | Neg (* Negation operator [-a] *)
  | Not (* Logical NOT [not a] *)
[@@deriving show { with_path = false }]

(* Type representing binary operations. *)
type bin_op =
  | Add (* Addition [a + b] *)
  | Sub (* Subtraction [a - b] *)
  | Mul (* Multiplication [a * b] *)
  | Div (* Division [a / b] *)
  | And (* Logical AND [a && b] *)
  | Or (* Logical OR [a || b] *)
  | Eq (* Equality [a == b] *)
  | Neq (* Inequality [a /= b] *)
  | Lt (* Less than [a < b] *)
  | Gt (* Greater than [a > b] *)
  | Leq (* Less than or equal to [a <= b] *)
  | Geq (* Greater than or equal to [a >= b] *)
[@@deriving show { with_path = false }]

(* Type representing literals *)
type lit =
  | LitInt of int (* Integer literal [7] *)
  | LitBool of bool (* Boolean literal [True] *)
  | LitChar of char (* Character literal ['a'] *)
  | LitString of string (* String literal ["abc"] *)
(* | LitFloat of float  * Float literal [3.14] *)
[@@deriving show { with_path = false }]

(* Type representing possible patterns in pattern-matching. *)
type pat =
  | PatWild (* Wildcard pattern [_] *)
  | PatVar of id (* Variable pattern [x] *)
  | PatLit of lit (* Literal pattern [1], ['a'], ["abc"] *)
  | PatTuple of pat list (* Tuple pattern [(x, y)] *)
  | PatCons of pat * pat (* List constructor pattern [x:xs] *)
  | PatNil (* Empty list pattern [[]] *)
  | PatLeaf (* Leaf node of a tree *)
  | PatTree of pat * pat * pat (* Tree pattern *)
[@@deriving show { with_path = false }]

(* Type representing binding from patterns to expressions. *)
type binding = pat * expr [@@deriving show { with_path = false }]

(* Type representing case alternative in pattern matching. *)
and alt = pat * expr [@@deriving show { with_path = false }]

(* Type representing possible expressions *)
and expr =
  | ExprLit of lit (* Literal value [1], ['a'], ["abc"] *)
  | ExprVar of id (* Variable reference [x] *)
  | ExprFunc of binding (* Function/closure definition [\x -> x + 1] *)
  | ExprApp of expr * expr (* Function application [f x] *)
  | ExprIf of expr * expr * expr (* If expression [if x then y else z] *)
  | ExprTuple of expr list (* Tuple [(x, y)] *)
  | ExprCons of expr * expr (* List construction [x:xs] *)
  | ExprNil (* Empty list [[]] *)
  | ExprCase of expr * alt list
    (* Case expression [case (m,ys) of
                          (0,_)       ->  []
                            (_,[])      ->  []
                            (n,x:xs)    ->  x : take (n-1) xs] *)
  | ExprBinOp of bin_op * expr * expr (* Binary operation [a + b] *)
  | ExprUnOp of un_op * expr (* Unary operation [not a] *)
  | ExprLet of binding list * expr (* Local binding [let x = 1 in x + 2] *)
  | ExprTree of tree (* Tree value [Node "l" (Node "o" Nil Nil) (Node "h" Nil Nil)] *)
[@@deriving show { with_path = false }]

(* Type representing a binary tree structure with value, left child and right child respectively *)
and tree =
  | Leaf
  | Node of expr * expr * expr

(* Type representing a single declaration. *)
type decl = DeclLet of binding (* Let binding declaration [x = 5] *)
[@@deriving show { with_path = false }]

(* Type representing a whole program as a series of declarations. *)
type prog = decl list (* Series of declarations [x = 5; y = 3] *)
[@@deriving show { with_path = false }]
