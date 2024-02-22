(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_reg_64 : [ `Reg_64 ] Ast.operand Angstrom.t
val parse_reg_128 : [ `Reg_128 ] Ast.operand Angstrom.t
val parse_imm_a : [ `Imm_a ] Ast.operand Angstrom.t
val parse_mem_64_mem_64 : [ `Mem_64_mem_64 ] Ast.operand Angstrom.t
val parse_reg_64_reg_64 : [ `Reg_64_reg_64 ] Ast.operand Angstrom.t
val parse_mem_64 : [ `Mem_64 ] Ast.operand Angstrom.t
val parse_label_operand : [ `Label ] Ast.operand Angstrom.t
val parse_nothing : [ `Nothing ] Ast.operand Angstrom.t
val parse_reg_64_imm_a : [ `Reg_64_imm_a ] Ast.operand Angstrom.t
val parse_reg_128_reg_128 : [ `Reg_128_reg_128 ] Ast.operand Angstrom.t
val parse_reg_128_reg_64 : [ `Reg_128_reg_64 ] Ast.operand Angstrom.t
val parse_reg_64_reg_128 : [ `Reg_64_reg_128 ] Ast.operand Angstrom.t
val parse_reg_128_reg_64_imm_a : [ `Reg_128_reg_64_imm_a ] Ast.operand Angstrom.t
