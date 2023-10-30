(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

type tp =
  (* only for function ret*)
  | TVoid (** void *)
  (* primitive types*)
  | TFloat (** float *)
  | TInteger of int (** i1, i2 ... in *)
  | TPointer (** ptr *)
  (* first class types*)
  | TVector of int * tp (** <int x primitive_type> *)
  | TArr of int * tp (** [int x type] *)
  | TStruct of tp list (** {type1, type2...} *)
  (* additional types*)
  | TLabel (** label *)
  | TFunc of tp * tp list (** <returntype> (<parameter list>) *)
(*
   | Token
   | Metadata
*)
[@@deriving show { with_path = false }, eq]

type variable =
  | LocalVar of string (** %name *)
  | GlobalVar of string (** @name *)
[@@deriving show { with_path = false }]

type pointer_const =
  | PointerGlob of variable
  | PointerInt of int
[@@deriving show { with_path = false }]

type align = int (* just for better reading*) [@@deriving show { with_path = false }]

type const =
  | CVoid
  | CInteger of int * int (** size and value*)
  | CFloat of float
  | CPointer of pointer_const
  | CVector of const list (** <const, const, ...> *)
  | CArr of const list (** [const, const, ...] *)
  | CStruct of const list (** {const, const, ...} *)
  | CLabel of basic_block
  | CFunc of func
[@@deriving show { with_path = false }]

and value =
  | FromVariable of variable * tp
  | Const of const
[@@deriving show { with_path = false }]

(* ############ Instructions Start ########### *)
and terminator_instruction =
  | Ret of tp * value (** ret <type> <value> *)
  | Br of value (** br label <dest> *)
  | BrCond of value * value * value (** br i1 <cond>, label <iftrue>, label <iffalse> *)
[@@deriving show { with_path = false }]

and binary_operation_body =
  variable * tp * value * value (* <result> = <bin_op> <ty> <val1>, <val2> *)
[@@deriving show { with_path = false }]

and binary_operation =
  | Mul of binary_operation_body
  | Sub of binary_operation_body
[@@deriving show { with_path = false }]

and other_operation =
  | Icmp of variable * string * tp * value * value
      (** <result> = icmp <cond> <ty> <op1>, <op2> *)
  | Call of variable * tp * value * value list
      (** <result> = call <ty> <fnptrval>(<function args>) *)

and memory_address_inst =
  | Alloca of variable * tp * value * align
      (** <result> = alloca <type> [, <ty> <NumElements>] [, align <alignment>] *)
  | Store of tp * value * value * align
      (** store <ty> <value>, ptr <pointer>[, align <alignment>] *)
  | Load of variable * tp * value * align
      (** <result> = load <ty>, ptr <pointer>[, align <alignment>]*)

and instruction =
  | Terminator of terminator_instruction
  | Binary of binary_operation
  | Other of other_operation
  | MemoryAddress of memory_address_inst
(* | Unary
   | BitwiseBinary
   | Vector
   | Aggregate
   | Conversion
*)
[@@deriving show { with_path = true }]
(* ############ Instructions End ########### *)

and basic_block = instruction list [@@deriving show { with_path = false }]

and func =
  { parameters : variable list
  ; basic_blocks : (variable * const) list
  }
[@@deriving show { with_path = false }]

type glob_list = (tp * variable * const) list [@@deriving show { with_path = false }]
