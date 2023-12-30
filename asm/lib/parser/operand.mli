(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_lable_ref : Ast.label_ref Angstrom.t
val parse_reg_32 : [ `Reg_32 ] Ast.operand Angstrom.t
val parse_reg_64 : [ `Reg_64 ] Ast.operand Angstrom.t
val parse_imm_a : [ `Imm_a ] Ast.operand Angstrom.t
val parse_mem_32_mem_32 : [ `Mem_32_mem_32 ] Ast.operand Angstrom.t
val parse_mem_64_mem_64 : [ `Mem_64_mem_64 ] Ast.operand Angstrom.t
val parse_reg_32_reg_32 : [ `Reg_32_reg_32 ] Ast.operand Angstrom.t
val parse_reg_64_reg_64 : [ `Reg_64_reg_64 ] Ast.operand Angstrom.t
val parse_mem_32 : [ `Mem_32 ] Ast.operand Angstrom.t
val parse_mem_64 : [ `Mem_64 ] Ast.operand Angstrom.t
val parse_label_operand : [ `Label ] Ast.operand Angstrom.t
val parse_nothing : [ `Nothing ] Ast.operand Angstrom.t
val parse_reg_32_imm_a : [ `Reg_32_imm_a ] Ast.operand Angstrom.t
val parse_reg_64_imm_a : [ `Reg_64_imm_a ] Ast.operand Angstrom.t
