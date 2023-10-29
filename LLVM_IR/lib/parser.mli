(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val test_parse : 'a Angstrom.t -> ('a -> string) -> string -> unit
val whitespace : char -> bool
val comment : unit Angstrom.t
val whitespaces : unit list Angstrom.t
val str_integer : string Angstrom.t
val integer : int Angstrom.t
val varname_char : char -> bool
val parse_word : string Angstrom.t
val word : string -> string Angstrom.t
val parse_primitive_type : Ast.tp Angstrom.t
val parse_main_type : Ast.tp Angstrom.t
val parse_additional_type : Ast.tp Angstrom.t
val parse_named_name : string Angstrom.t
val parse_unnamed_name : string Angstrom.t
val parse_name : string Angstrom.t
val parse_local_variable : Ast.variable Angstrom.t
val parse_global_variable : Ast.variable Angstrom.t
val parse_const : Ast.tp -> Ast.const Angstrom.t
val parse_const_aggregate_type :
  char -> char -> Ast.tp list -> Ast.const list Angstrom.t
val parse_const_array : int -> Ast.tp -> Ast.const Angstrom.t
val parse_const_pointer : Ast.const Angstrom.t
val parse_const_float : Ast.const Angstrom.t
val parse_const_integer : int -> Ast.const Angstrom.t
val parse_value : Ast.tp -> Ast.value Angstrom.t
val additional_type : Ast.tp -> Ast.tp Angstrom.t
val parse_type_with_value : (Ast.tp * Ast.value) Angstrom.t
val parse_type_with_value2 : (Ast.tp * Ast.value * Ast.value) Angstrom.t
val type_with_value : Ast.tp -> Ast.value Angstrom.t
val parse_instruction_result : Ast.variable Angstrom.t
val parse_terminator_instruction : Ast.terminator_instruction Angstrom.t
val parse_binary_operation : Ast.binary_operation Angstrom.t
val parse_other_operation : Ast.other_operation Angstrom.t
val parse_memory_instruction : Ast.memory_address_inst Angstrom.t
val parse_instruction : Ast.instruction Angstrom.t
type func_annotation = {
  self : Ast.variable;
  tp : Ast.tp;
  parameters : Ast.variable list;
}
val pp_func_annotation : Format.formatter -> func_annotation -> unit
val show_func_annotation : func_annotation -> string
val parse_function_annotation : func_annotation Angstrom.t
val parse_basic_block_variable : Ast.variable Angstrom.t
val parse_basic_block_body : Ast.instruction list Angstrom.t
val parse_start_basic_block : (Ast.variable * Ast.const) Angstrom.t
val parse_basic_block : (Ast.variable * Ast.const) Angstrom.t
val parse_function_body : (Ast.variable * Ast.const) list Angstrom.t
val parse_function : (Ast.tp * Ast.variable * Ast.const) Angstrom.t
val start_parse : Ast.glob_list Angstrom.t
val parse_programm : string -> (string, string) result
