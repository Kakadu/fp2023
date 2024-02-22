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
  | TStruct of tp list (** \{type1, type2...\} *)
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
  | GlobalVar of string (** \@name *)
[@@deriving show { with_path = false }]

type pointer_const =
  | PointerGlob of variable
  | PointerInt of int
[@@deriving show { with_path = false }]

type align = int (* just for better reading*) [@@deriving show { with_path = false }]

type const =
  | CVoid
  | CInteger of int * Int64.t (** size and value*)
  | CFloat of float
  | CPointer of pointer_const
  | CVector of const list (** <const, const, ...> *)
  | CArr of const list (** [const, const, ...] *)
  | CStruct of const list (** \{const, const, ...\} *)
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
  | Switch of tp * value * value * (value * value) list
  (** switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ] *)
  | Unreachable (** unreachable *)
[@@deriving show { with_path = false }]

and unary_operation = Fneg of variable * tp * value (** <result> = fneg <ty> <op1> *)
[@@deriving show { with_path = false }]

and binary_operation_body =
  variable * tp * value * value (* <result> = <bin_op> <ty> <val1>, <val2> *)
[@@deriving show { with_path = false }]

and binary_operation =
  | Add of binary_operation_body
  | Fadd of binary_operation_body
  | Mul of binary_operation_body
  | Fmul of binary_operation_body
  | Sub of binary_operation_body
  | Fsub of binary_operation_body
  | Udiv of binary_operation_body
  | Sdiv of binary_operation_body
  | Fdiv of binary_operation_body
  | Urem of binary_operation_body
  | Srem of binary_operation_body
  | Frem of binary_operation_body
[@@deriving show { with_path = false }]

and bitwise_binary_operation =
  | Shl of binary_operation_body
  | Lshr of binary_operation_body
  | Ashr of binary_operation_body
  | And of binary_operation_body
  | Or of binary_operation_body
  | Xor of binary_operation_body
[@@deriving show { with_path = false }]

and vector_instruction =
  | Extractelement of variable * tp * value * tp * value
  (** <result> = extractelement <n x <ty>> <val>, <ty2> <idx> *)
  | Insertelement of variable * tp * value * value * tp * value
  (** <result> = insertelement <n x <ty>> <val>, <ty> <elt>, <ty2> <idx> *)
  | Shufflevector of variable * tp * value * value * int * const
  (** <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask> *)
[@@deriving show { with_path = false }]

and aggregate_instruction =
  | Extractvalue of variable * tp * value * int list
  (** <result> = extractvalue <aggregate type> <val>, <idx>\{, <idx>\}* *)
  | Insertvalue of variable * tp * value * tp * value * int list
  (** <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>\{, <idx>\}* *)

and memory_address_instruction =
  | Alloca of variable * tp * value * align
  (** <result> = alloca <type> [, <ty> <NumElements>] [, align <alignment>] *)
  | Store of tp * value * value * align
  (** store <ty> <value>, ptr <pointer>[, align <alignment>] *)
  | Load of variable * tp * value * align
  (** <result> = load <ty>, ptr <pointer>[, align <alignment>]*)
  | Getelementptr of variable * tp * tp * value * (tp * value) list
  (** <result> = getelementptr <ty>, <ptr_or_ptr_vector> <ptrval>\{, <ty> <idx>\}* *)

(** <result> = <conv_inst> <ty> <value> to <ty2> *)
and conversion_instruction_body = variable * tp * value * tp

and conversion_instruction =
  | TruncTo of conversion_instruction_body
  | ZextTo of conversion_instruction_body
  | SextTo of conversion_instruction_body
  | FptouiTo of conversion_instruction_body
  | FptosiTo of conversion_instruction_body
  | UitofpTo of conversion_instruction_body
  | SitofpTo of conversion_instruction_body
  | PrttointTo of conversion_instruction_body
  | InttoprtTo of conversion_instruction_body

and other_operation =
  | Icmp of variable * string * tp * value * value
  (** <result> = icmp <cond> <ty> <op1>, <op2> *)
  | Fcmp of variable * string * tp * value * value
  (** <result> = fcmp <cond> <ty> <op1>, <op2> *)
  | Phi of variable * tp * (value * value) list
  (** <result> = phi <ty> [ <val0>, <label0>], ... *)
  | Select of variable * tp * value * tp * value * value
  (** <result> = select <i1 or i1_vec> <cond>, <ty> <val1>, <ty> <val2>*)
  | Call of variable * tp * value * value list
  (** <result> = call <ty> <fnptrval>(<function args>) *)

and instruction =
  | Terminator of terminator_instruction
  | Unary of unary_operation
  | Binary of binary_operation
  | BitwiseBinary of bitwise_binary_operation
  | Vector of vector_instruction
  | Aggregate of aggregate_instruction
  | Other of other_operation
  | MemoryAddress of memory_address_instruction
  | Conversion of conversion_instruction
[@@deriving show { with_path = true }]
(* ############ Instructions End ########### *)

and basic_block = instruction list [@@deriving show { with_path = false }]

and func =
  { ftp : tp
  ; parameters : variable list
  ; basic_blocks : (variable * const) list
  }
[@@deriving show { with_path = false }]

type glob_list = (tp * variable * const * int) list
[@@deriving show { with_path = false }]

let rec const_to_tp : const -> tp = function
  | CVoid -> TVoid
  | CInteger (size, _) -> TInteger size
  | CFloat _ -> TFloat
  | CPointer _ -> TPointer
  | CVector lst ->
    (match lst with
     | h :: _ -> TVector (List.length lst, const_to_tp h)
     | [] -> TVector (List.length lst, TVoid))
  | CArr lst ->
    (match lst with
     | h :: _ -> TArr (List.length lst, const_to_tp h)
     | [] -> TArr (List.length lst, TVoid))
  | CStruct lst -> TStruct (List.map const_to_tp lst)
  | CLabel _ -> TLabel
  | CFunc f -> f.ftp
;;

let rec tp_equal : tp -> tp -> bool =
  fun t1 t2 ->
  match t1, t2 with
  | TVoid, TVoid | TLabel, TLabel | TFloat, TFloat | TPointer, TPointer -> true
  | TInteger x1, TInteger x2 when x1 = x2 -> true
  | (TVector (n1, tp1), TVector (n2, tp2) | TArr (n1, tp1), TArr (n2, tp2))
    when n1 = n2 && tp_equal tp1 tp2 -> true
  | TStruct tps1, TStruct tps2 -> List.equal tp_equal tps1 tps2
  | _ -> false
;;

let variable_equal v1 v2 =
  match v1, v2 with
  | GlobalVar n1, GlobalVar n2 | LocalVar n1, LocalVar n2 -> String.equal n1 n2
  | _, _ -> false
;;
