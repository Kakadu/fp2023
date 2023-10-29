(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

type tp =
    TVoid
  | TFloat
  | TInteger of int
  | TPointer
  | TVector of int * tp
  | TArr of int * tp
  | TStruct of tp list
  | TLabel
  | TFunc of tp * tp list
val pp_tp : Format.formatter -> tp -> unit
val show_tp : tp -> string
val equal_tp : tp -> tp -> bool
type variable = LocalVar of string | GlobalVar of string
and pointer_const = PointerGlob of variable | PointerInt of int
and const =
    CVoid
  | CInteger of int * int
  | CFloat of float
  | CPointer of pointer_const
  | CVector of const list
  | CArr of const list
  | CStruct of const list
  | CLabel of basic_block
  | CFunc of func
and value = FromVariable of variable * tp | Const of const
and terminator_instruction =
    Ret of tp * value
  | Br of value
  | BrCond of value * value * value
and binary_operation_body = variable * tp * value * value
and binary_operation =
    Mul of binary_operation_body
  | Sub of binary_operation_body
and other_operation =
    Icmp of variable * string * tp * value * value
  | Call of variable * tp * value * value list
and align = int
and memory_address_inst =
    Alloca of variable * tp * value * align
  | Store of tp * value * value * align
  | Load of variable * tp * value * align
and instruction =
    Terminator of terminator_instruction
  | Binary of binary_operation
  | Other of other_operation
  | MemoryAddress of memory_address_inst
and basic_block = instruction list
and func = {
  parameters : variable list;
  basic_blocks : (variable * const) list;
}
val pp_variable : Format.formatter -> variable -> unit
val show_variable : variable -> string
val pp_pointer_const : Format.formatter -> pointer_const -> unit
val show_pointer_const : pointer_const -> string
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string
val pp_value : Format.formatter -> value -> unit
val show_value : value -> string
val pp_terminator_instruction :
  Format.formatter -> terminator_instruction -> unit
val show_terminator_instruction : terminator_instruction -> string
val pp_binary_operation_body :
  Format.formatter -> binary_operation_body -> unit
val show_binary_operation_body : binary_operation_body -> string
val pp_binary_operation : Format.formatter -> binary_operation -> unit
val show_binary_operation : binary_operation -> string
val pp_other_operation : Format.formatter -> other_operation -> unit
val show_other_operation : other_operation -> string
val pp_align : Format.formatter -> align -> unit
val show_align : align -> string
val pp_memory_address_inst : Format.formatter -> memory_address_inst -> unit
val show_memory_address_inst : memory_address_inst -> string
val pp_instruction : Format.formatter -> instruction -> unit
val show_instruction : instruction -> string
val pp_basic_block : Format.formatter -> basic_block -> unit
val show_basic_block : basic_block -> string
val pp_func : Format.formatter -> func -> unit
val show_func : func -> string
type glob_list = (tp * variable * const) list
val pp_glob_list : Format.formatter -> glob_list -> unit
val show_glob_list : glob_list -> string
