(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Common
open Register
open Immediate

let parse_reg_64 = reg_64 <$> parse_64_register
let parse_reg_128 = reg_128 <$> parse_128_register
let parse_imm_a = imm_a <$> parse_immidiate

let parse_mem_64_mem_64 =
  lift2 mem_64_mem_64 parse_64_register_ref (comma *> parse_64_register_ref)
;;

let parse_reg_64_reg_64 =
  lift2 reg_64_reg_64 parse_64_register (comma *> parse_64_register)
;;

let parse_mem_64 = mem_64 <$> parse_64_register_ref
let parse_label_operand = label <$> label_name
let parse_nothing = return nothing
let parse_reg_64_imm_a = lift2 reg_64_imm_a parse_64_register (comma *> parse_immidiate)

let parse_reg_128_reg_128 =
  lift2 reg_128_reg_128 parse_128_register (comma *> parse_128_register)
;;

let parse_reg_128_reg_64 =
  lift2 reg_128_reg_64 parse_128_register (comma *> parse_64_register)
;;

let parse_reg_64_reg_128 =
  lift2 reg_64_reg_128 parse_64_register (comma *> parse_128_register)
;;

let parse_reg_128_reg_64_imm_a =
  lift3
    reg_128_reg_64_imm_a
    parse_128_register
    (comma *> parse_64_register)
    (comma *> parse_immidiate)
;;
