(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Angstrom
open Ast

let parse_32_register =
  choice
    ~failure_msg:"parse_32_register"
    [ string_ci "eax" *> return eax
    ; string_ci "ebx" *> return ebx
    ; string_ci "ecx" *> return ecx
    ; string_ci "edx" *> return edx
    ; string_ci "esp" *> return esp
    ; string_ci "ebp" *> return ebp
    ; string_ci "esi" *> return esi
    ; string_ci "edi" *> return edi
    ]
;;

let parse_64_register =
  choice
    ~failure_msg:"parse_64_register"
    [ string_ci "rax" *> return rax
    ; string_ci "rbx" *> return rbx
    ; string_ci "rcx" *> return rcx
    ; string_ci "rdx" *> return rdx
    ; string_ci "rsp" *> return rsp
    ; string_ci "rbp" *> return rbp
    ; string_ci "rsi" *> return rsi
    ; string_ci "rdi" *> return rdi
    ]
;;

let between_brackets parser = between (char '[') (char ']') parser <?> "between_brackets"

let parse_32_register_ref =
  between_brackets parse_32_register >>| register_ref <?> "parse_32_register_ref"
;;

let parse_64_register_ref =
  between_brackets parse_64_register >>| register_ref <?> "parse_64_register_ref"
;;
