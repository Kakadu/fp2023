(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type un_op =
  | PreInc (*"++", precedence 14*)
  | PreDec (*"--", precedence 14*)
  | Plus (*"+", precedence 14*)
  | Minus (*"-", precedence 14*)
  | LogicalNot (*"!", precedence 14*)
  | BitwiseNot (*"~", precedence 14*)
  | TypeOf (*"typeof", precedence 14*)
  | PostInc (*"++", precedence 15*)
  | PostDec (*"--", precedence 15*)
  | New (*"new", precedence 16*)
[@@deriving show { with_path = false }]

type bin_op =
  | Assign (*"=", precedence 2*)
  | AddAssign (*"+=", precedence 2*)
  | SubAssign (*"-=", precedence 2*)
  | MulAssign (*"*=", precedence 2*)
  | DivAssign (*"/=", precedence 2*)
  | ExpAssign (*"**=", precedence 2*)
  | RemAssign (*"%=", precedence 2*)
  | BitAndAssign (*"&=", precedence 2*)
  | BitXorAssign (*"^=", precedence 2*)
  | BitOrAssign (*"|=", precedence 2*)
  | LShiftAssign (*"<<=", precedence 2*)
  | RShiftAssign (*">>=", precedence 2*)
  | URShiftAssign (*">>>=", precedence 2*)
  | LogAndAssign (*"&&=", precedence 2*)
  | LogOrAssign (*"||=", precedence 2*)
  | NullAssign (*"??=", precedence 2*)
  | LogicalOr (*"||", precedence 3*)
  | NullishCoal (*"??", precedence 3*)
  | LogicalAnd (*"&&", precedence 4*)
  | BitwiseOr (*"|", precedence 5*)
  | Xor (*"^", precedence 6*)
  | BitwiseAnd (*"&", precedence 7*)
  | Equal (*"==", precedence 8*)
  | NotEqual (*"!=", precedence 8*)
  | StrictEqual (*"===", precedence 8*)
  | StrictNotEqual (*"!==", precedence 8*)
  | GreaterThan (*">", precedence 9*)
  | GreaterEqual (*">=", precedence 9*)
  | LessThan (*"<", precedence 9*)
  | LessEqual (*"<=", precedence 9*)
  | LogicalShiftLeft (*"<<", precendence 10*)
  | LogicalShiftRight (*">>", precendence 10*)
  | UnsignedShiftRight (*">>>", precendence 10*)
  | Add (*"+", precendence 11*)
  | Sub (*"-", precendence 11*)
  | Mul (*"%", precendence 12*)
  | Div (*"/", precendence 12*)
  | Rem (*"%", precendence 12*)
  | Exp (*"**", precendence 13*)
  | PropAccs (*x[y], precedence 17*)
[@@deriving show { with_path = false }]

type typename =
  | Number of float
  | String of string
  | Bool of bool
  | Undefined
  | Null
[@@deriving show { with_path = false }]

type expression =
  | UnOp of un_op * expression
  | BinOp of bin_op * expression * expression
  | Const of typename
  | Var of string
  | FunctionCall of expression * expression list
  | AnonFunction of string list * statement (*objects like: function (args) {...}*)
  | ArrowFunction of
      string list * statement (*objects like: (args)=>... or (args)=>{...}*)
  | ArrayList of expression list (*objects like: [val, val, ...]*)
  | ObjectDef of (expression * expression) list (*objects like: { key : val, ...}*)
[@@deriving show { with_path = false }]

and var_init =
  { var_identifier : string
  ; is_const : bool
  ; value : expression
  }

and fun_init =
  { fun_identifier : string
  ; arguments : string list
  ; body : statement
  }

and loop =
  { loop_init : statement option (*in 'for' only*)
  ; loop_condition : expression
  ; loop_change : expression option (*in 'for' only*)
  ; loop_body : statement
  }

and statement =
  | Block of statement list (*{...}*)
  | Expression of expression
  | VarInit of var_init (*let id = ...*)
  | FunInit of fun_init (*function id(args) {...}*)
  | If of expression * statement * statement
  | Loop of loop (*'for' or 'while' loop*)
  | Return of expression
  | Programm of statement list
[@@deriving show { with_path = false }]
