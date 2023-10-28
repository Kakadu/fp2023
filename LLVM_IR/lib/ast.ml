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

and pointer_const = 
| PointerGlob of variable
| PointerInt of int
[@@deriving show { with_path = false }]


and const =
  | CVoid
  | CInteger of int * int (** size and value*)
  | CFloat of float
  | CPointer of pointer_const
  | CVector of const list
  | CArr of const list
  | CStruct of const list
  | CLabel of basic_block
  | CFunc of func
[@@deriving show { with_path = false }]

and value =
  | FromVariable of variable*tp
  | Const of const
[@@deriving show { with_path = false }]

(* ############ Instructions Start ########### *)
and terminator_instruction =
  | Ret of tp * value (** ret <type> <value> *)
  | Br of value (** br label <dest> *)
  | BrCond of value * value * value (** br i1 <cond>, label <iftrue>, label <iffalse> *)
[@@deriving show { with_path = false }]

  and binary_operation_body =
  variable * tp * value * value (* <result> = bin_op <ty> <val1>, <val2> *)
[@@deriving show { with_path = false }]

and binary_operation =
  | Mul of binary_operation_body
  | Sub of binary_operation_body
[@@deriving show { with_path = false }]

and instruction =
  | Terminator of terminator_instruction
  | Binary of binary_operation
  | Other
  | MemoryAddress
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
  ; basic_blocks : variable * basic_block list
  }
[@@deriving show { with_path = false }]

type glob_list = (tp * variable * value) list [@@deriving show { with_path = false }]
