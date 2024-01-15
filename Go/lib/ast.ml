(** Copyright 2021-2023, Nikita Lukonenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


type type' =
  | StringType [@foo] (* string type *)
  | IntegerType  (* integer type *)
  | BooleanType  (* boolean type *)
  | FuncType of signature (* function type *)
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here are the data types that are used in the Golang language." *)

and signature =
  { args : arg list
  ; ret : ret_typ
  }
[@@deriving show { with_path = false }]
(** @ocaml.doc "The structure of the function signature is indicated here. args is the list of arguments that the function accepts, ret is the return type of the function." *)


and arg = string * type' 
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here it is indicated what kind of function argument they have. It consists of a pair of string(argument name) - type of argument." *)

and ret_typ =
  | Type of type' (* the type of function that returns something *)
  | Void (* the type of function that returns nothing *)
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
  | Const of identifier
  | Identifier of string
  | BinaryOp of (expression * binary_operation * expression) (* (a < b) *)
  | UnaryOp of (unaryop * expression) (* unop expr *)
  | FunCall of expression * expression list
  | AnonFunc of (signature * block)
  | PrintCall of expression list
  | LenCall of expression
[@@deriving show { with_path = false }]
(** @ocaml.doc "All types of expressions provided by this Golang language interpreter are listed here.
    Lan Call is a constructor of a predefined len function that accepts only a string constant as input." *)
and


(* both constants and identifier are listed in identifier *)
identifier =
  | Integer of int  (* integer identifier *)
  | String of string  (* string identifier *)  
  | Bool of bool  (*boolean identifier*)
  | Nil (*Nil identifier*)
(** @ocaml.doc "All types of identifiers provided by this Golang language interpreter are listed here." *)


and var_declaration =  string * type' * expression
(** @ocaml.doc "Here is a constructor that represents the structure of defining a variable." *)

and func_declaration = string * signature * block
(** @ocaml.doc "Here is a constructor that represents the structure of the function definition." *)

and global_declaration = 
  | VarTopLevelDeclaration of var_declaration
  | FuncTopLevelDeclaration  of func_declaration
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here is a type that represents the constructor of the global definition." *)

and block = statements list
(** @ocaml.doc "Here is a constructor, which is a block of a sequence of instructions." *)

and else_ = block
(** @ocaml.doc "Here is a constructor that represents the else block for the if block." *)

and statements =
  | IfElseStatement of expression * block * else_
  | ForStatement of expression * block
  | ReturnStatement of expression option
  | BlockStatement of block
  | ExpressinStatement of expression
  | AssignStatement of expression * expression
  | VarStatement of var_declaration
  | BreakStatement
  | ContinueStatement
[@@deriving show { with_path = false }]
(** @ocaml.doc "Here is a type that contains constructors of statements." *)

type file = global_declaration list [@@deriving show { with_path = false }]
