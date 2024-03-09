(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | Char of char (** 'a' *)
  | String of string (** "string" *)
  | Int of int (** 1 *)
  | Bool of bool (** true *)
  | Unit (** () *)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (**  +  *)
  | Sub (**  -  *)
  | Mul (**  * *)
  | Div (**  / *)
  | Eq (**  = *)
  | NEq (**  <> or != *)
  | Gt (**  > *)
  | Gte (**  >= *)
  | Lt (**  < *)
  | Lte (**  <= *)
  | And (**  && *)
  | Or (**  || *)
[@@deriving show { with_path = false }]

type un_op =
  | Not (**  not *)
  | Minus (**  -  *)
  | Plus (**  +  *)
[@@deriving show { with_path = false }]

type effect_types_annotation =
  | AInt (** int *)
  | ABool (** bool *)
  | AChar (** char *)
  | AString (** string *)
  | AUnit (** unit *)
  | AArrow of effect_types_annotation * effect_types_annotation (** int -> char *)
  | ATuple of effect_types_annotation list (** int * char * bool *)
  | AList of effect_types_annotation (** int list *)
  | AEffect of effect_types_annotation (** int effect *)
[@@deriving show { with_path = false }]

type continue_val = Continue of id (** continue k 0 - here k is continue variable *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** _ *)
  | PNill (** [] *)
  | PConst of const (** 1 *)
  | PVal of id (** x *)
  | PTuple of pattern list (** (x, y, z) *)
  | PListCons of pattern * pattern (** hd :: tl *)
  | PEffectWithArguments of id * pattern (** Effect (x::y) *)
  | PEffectWithoutArguments of id (** Effect *)
[@@deriving show { with_path = false }]

type effect_handler =
  | EffectHandler of pattern * expr * continue_val
  (** try x with | E k -> continue k 0 - the part after with is the effect handler *)
[@@deriving show { with_path = false }]

and expr =
  | EConst of const (** 1 *)
  | EBinaryOperation of bin_op * expr * expr (** 1 + 1 *)
  | EUnaryOperation of un_op * expr (** -1, not true *)
  | EIdentifier of id (** let f = 5 - here f is identifier *)
  | EApplication of expr * expr (** f x *)
  | EFun of pattern * expr (** fun x -> x *)
  | ELetIn of id * expr * expr (** let f = 1 in E *)
  | ERecLetIn of id * expr * expr (** let rec f x = f (x-1) in E *)
  | EEffectWithArguments of id * expr (** DevisionByZero 0 *)
  | EEffectWithoutArguments of id (** DevisionByZero *)
  | EIfThenElse of expr * expr * expr (** if x = 1 then x else x - 1 *)
  | EList of expr list (** [1 ; 2 ; 3] *)
  | EListCons of expr * expr (** 1 :: 2 :: 3 :: [] *)
  | ETuple of expr list (** (1, true, 'c') *)
  | EMatchWith of expr * (pattern * expr) list
  (** match x with | hd :: tl -> hd | _ -> 0 *)
  | ETryWith of expr * effect_handler list
  (** try x with | Effect1 k -> continue k 0 | Effect2 k -> continue k 1 *)
  | EEffectPerform of expr (** perform DevisionByZero *)
  | EEffectContinue of continue_val * expr (** continue k 0 *)
[@@deriving show { with_path = false }]

type decl =
  | DDeclaration of id * expr (** let f = 1 *)
  | DRecDeclaration of id * expr (** let rec f x = f (x-1) *)
  | DEffectDeclaration of id * effect_types_annotation
  (** effect DevisionByZero : int effect *)
[@@deriving show { with_path = false }]

type struct_item =
  | SDeclaration of decl (** Consructor for declarations *)
  | SExpression of expr (** Constructor for expressions *)
[@@deriving show { with_path = false }]

type program = struct_item list [@@deriving show { with_path = false }]
