(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Angstrom
open Ast

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

let parse_128_register =
  choice
    ~failure_msg:"parse_128_register"
    [ string_ci "xmm0" *> return xmm0
    ; string_ci "xmm1" *> return xmm1
    ; string_ci "xmm2" *> return xmm2
    ; string_ci "xmm3" *> return xmm3
    ; string_ci "xmm4" *> return xmm4
    ; string_ci "xmm5" *> return xmm5
    ; string_ci "xmm6" *> return xmm6
    ; string_ci "xmm7" *> return xmm7
    ]
;;

let between_brackets parser = between (char '[') (char ']') parser <?> "between_brackets"

let parse_64_register_ref =
  between_brackets parse_64_register >>| register_ref <?> "parse_64_register_ref"
;;

let parse_128_register_ref =
  between_brackets parse_128_register >>| register_ref <?> "parse_128_register_ref"
;;
