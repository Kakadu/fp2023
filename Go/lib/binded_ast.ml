(** Copyright 2021-2023, Nikita Lukonenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

type typeB' = type'
[@@deriving show { with_path = false }]

(* and id = string *)
(** @ocaml.doc "Here are the data types that are used in the Golang language." *)

and signature =
  { argsB : arg list
  ; retB : Ast.ret_typ
  }
[@@deriving show { with_path = false }]
(** @ocaml.doc "The structure of the function signature is indicated here. args is the list of arguments that the function accepts, ret is the return type of the function." *)


and arg = int * string * type' 
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here it is indicated what kind of function argument they have. It consists of a pair of string(argument name) - type of argument." *)

and ret_typ =
  | Type of typeB' (* the type of function that returns something *)
  | VoidB (* the type of function that returns nothing *)
[@@deriving show { with_path = false }]
(** @ocaml.doc "It specifies which types the function's return values can have. They can be one of the type' elements, or the function can return nothing and have the Void type." *)



type binary_operation =
  | MinusBin  (* binary subtraction operation *)
  | Asterisk  (* binary multiplication operation *)
  | Plus (* binary addition operation  *)
  | Division (* binary division operation *)
  | Equal  (* binary equality operation *)
  | NotEqual 
  | Less 
  | LessEqual 
  | Greater 
  | GreaterEqual 
  | And 
  | Or 
[@@deriving show { with_path = false }]
(** @ocaml.doc "All types of binary operations provided by this Golang language interpreter are listed here." *)

type unaryop =
  | MinusUn (* -expr *)
  | Not (* !expr *)
[@@deriving show { with_path = false }]
(** @ocaml.doc "All types of unary operations provided by this Golang language interpreter are listed here." *)

type expression =
  | ConstB of identifierB
  | IdentifierB of int * string
  | BinaryOpB of expression * binary_operation * expression (* (a < b) *)
  | UnaryOpB of (unaryop * expression) (* unop expr *)
  | FunCallB of expression * expression list
  | AnonFuncB of (signature * block)
  | PrintCallB of expression list
  | LenCallB of expression
[@@deriving show { with_path = false }]
(** @ocaml.doc "All types of expressions provided by this Golang language interpreter are listed here.
    Lan Call is a constructor of a predefined len function that accepts only a string constant as input." *)
and


(* both constants and identifierB are listed in identifierB *)
identifierB = identifier
  (* | Integer of int  (* integer identifierB *)
  | String of string  (* string identifierB *)  
  | Bool of bool  (*boolean identifierB*)
  | Nil Nil identifierB *)
(** @ocaml.doc "All types of identifiers provided by this Golang language interpreter are listed here." *)


and var_declaration = int * string * type' * expression
(** @ocaml.doc "Here is a constructor that represents the structure of defining a variable." *)

and func_declaration = int * string * signature * block
(** @ocaml.doc "Here is a constructor that represents the structure of the function definition." *)

and global_func_declaration =
  | FuncTopLevelDeclarationB of func_declaration
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here is a type that represents the constructor of the global function definition." *)

and global_var_declaration = 
  | VarTopLevelDeclarationB of var_declaration
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here is a type that represents the constructor of the global variable definition." *)


and block = statements list
(** @ocaml.doc "Here is a constructor, which is a block of a sequence of instructions." *)

and else_ = block
(** @ocaml.doc "Here is a constructor that represents the else block for the if block." *)

and statements =
  | IfElseStatementB of expression * block * else_
  | ForStatementB of expression * block
  | ReturnStatementB of expression option
  | BlockStatementB of block
  | ExpressinStatementB of expression
  | AssignStatementB of expression * expression
  | VarStatementB of var_declaration
  | BreakStatementB
  | ContinueStatementB
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here is a type that contains constructors of statements." *)

