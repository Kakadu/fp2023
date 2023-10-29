(** Copyright 2023-2024 Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type i8 =
  | Ah
  | Al
  | Bh
  | Bl
  | Ch
  | Cl
  | Dh
  | Dl
[@@deriving show { with_path = false }, variants]

type i16 =
  | Ax
  | Bx
  | Cx
  | Dx
[@@deriving show { with_path = false }, variants]

type i32 =
  | Eax
  | Ebx
  | Ecx
  | Edx
  | Esi
  | Edi
  | Esp
  | Ebp
[@@deriving show { with_path = false }, variants]

type i64 =
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsp
  | Rbp
  | Rsi
  | Rdi
[@@deriving show { with_path = false }, variants]

type reg =
  | I8 of i8
  | I16 of i16
  | I32 of i32
  | I64 of i64
[@@deriving show { with_path = false }, variants]

type mnemonic =
  | Mov
  | Push
  | Call
  | Add
  | Xor
  | Syscall
  | Jle
  | Dec
  | IMul
  | Pop
  | Jmp
  | Ret
  | Sub
  | Cmp
  | Je  
[@@deriving show { with_path = false }, variants]

type value =
  | NumVal of int
  | StrVal of string
[@@deriving show { with_path = false }, variants]

type reg_ref = Ref of reg [@@deriving show { with_path = false }, variants]

type operand1 =
  | Reg of reg
  | Imm of value
  | RegRef of reg_ref
  | LableRef of string
[@@deriving show { with_path = false }, variants]

type instruction =
  | InstrOperand2 : mnemonic * operand1 * operand1 -> instruction
  | InstrOperand1 : mnemonic * operand1 -> instruction
  | InstrOperand0 : mnemonic -> instruction
[@@deriving show { with_path = false }, variants]

type directive =
  | Section of string
  | Global of string
[@@deriving show { with_path = false }, variants]

type statement =
  | Instruction of instruction
  | Directive of directive
  | Label of string
[@@deriving show { with_path = false }, variants]

type ast = statement list [@@deriving show { with_path = false }]
