(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Ast
open Directive
open Angstrom
open Operand

let parse_label_decl = label_decl <$> label_name <* char ':'

let parse_instruction =
  let gen_instruction mnemonic choices =
    string_ci mnemonic
    *> spaces
    *> choice ~failure_msg:("No more operand choises for " ^ mnemonic) choices
  in
  choice
    [ gen_instruction "mov" [ instruction mov <$> parse_reg_64_imm_a ]
    ; gen_instruction "push" [ instruction push <$> parse_reg_64 ]
    ; gen_instruction "add" [ instruction add <$> parse_reg_64_reg_64 ]
    ; gen_instruction "xor" [ instruction xor <$> parse_reg_64_reg_64 ]
    ; gen_instruction "syscall" [ instruction syscall <$> parse_nothing ]
    ; gen_instruction "pop" [ instruction pop <$> parse_reg_64 ]
    ; gen_instruction "jmp" [ instruction jmp <$> parse_label_operand ]
    ; gen_instruction "sub" [ instruction sub <$> parse_reg_64_imm_a ]
    ; gen_instruction "cmp" [ instruction cmp <$> parse_reg_64_imm_a ]
    ; gen_instruction "jle" [ instruction jle <$> parse_label_operand ]
    ; gen_instruction "jne" [ instruction jne <$> parse_label_operand ]
    ; gen_instruction "je" [ instruction je <$> parse_label_operand ]
    ; gen_instruction
        "movq"
        [ instruction movq <$> parse_reg_128_reg_64
        ; instruction movq <$> parse_reg_64_reg_128
        ]
    ; gen_instruction "movapd" [ instruction movapd <$> parse_reg_128_reg_128 ]
    ; gen_instruction "punpckhqdq" [ instruction punpckhqdq <$> parse_reg_128_reg_128 ]
    ; gen_instruction "addpd" [ instruction addpd <$> parse_reg_128_reg_128 ]
    ; gen_instruction "pinsrq" [ instruction pinsrq <$> parse_reg_128_reg_64_imm_a ]
    ; gen_instruction "mulpd" [ instruction mulpd <$> parse_reg_128_reg_128 ]
    ; gen_instruction "haddpd" [ instruction haddpd <$> parse_reg_128_reg_128 ]
    ]
;;

let parse_statement =
  choice
    ~failure_msg:"parse_statement"
    [ parse_instruction; directive <$> parse_directive; parse_label_decl ]
;;
