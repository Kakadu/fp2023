(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Common
open Register
open Immediate

let parse_lable_ref = label_ref <$> label_name
let parse_reg_32 = reg_32 <$> parse_32_register
let parse_reg_64 = reg_64 <$> parse_64_register
let parse_imm_a = imm_a <$> parse_immidiate

let parse_mem_32_mem_32 =
  lift2 mem_32_mem_32 parse_32_register_ref (comma *> parse_32_register_ref)
;;

let parse_mem_64_mem_64 =
  lift2 mem_64_mem_64 parse_64_register_ref (comma *> parse_64_register_ref)
;;

let parse_reg_32_reg_32 =
  lift2 reg_32_reg_32 parse_32_register (comma *> parse_32_register)
;;

let parse_reg_64_reg_64 =
  lift2 reg_64_reg_64 parse_64_register (comma *> parse_64_register)
;;

let parse_mem_32 = mem_32 <$> parse_32_register_ref
let parse_mem_64 = mem_64 <$> parse_64_register_ref
let parse_label_operand = label <$> parse_lable_ref
let parse_nothing = return nothing
let parse_reg_32_imm_a = lift2 reg_32_imm_a parse_32_register (comma *> parse_immidiate)
let parse_reg_64_imm_a = lift2 reg_64_imm_a parse_64_register (comma *> parse_immidiate)
