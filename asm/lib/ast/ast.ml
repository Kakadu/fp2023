(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type i64
type i128

type _ register =
  | Rax : i64 register
  | Rbx : i64 register
  | Rcx : i64 register
  | Rdx : i64 register
  | Rsp : i64 register
  | Rbp : i64 register
  | Rsi : i64 register
  | Rdi : i64 register
  | Xmm0 : i128 register
  | Xmm1 : i128 register
  | Xmm2 : i128 register
  | Xmm3 : i128 register
  | Xmm4 : i128 register
  | Xmm5 : i128 register
  | Xmm6 : i128 register
  | Xmm7 : i128 register
[@@deriving variants]

type _ immediate = Imm_int of int [@@deriving variants]
type 'a register_ref = Register_ref of 'a register [@@deriving variants]

type _ operand =
  | Reg_64_reg_64 : i64 register * i64 register -> [> `Reg_64_reg_64 ] operand
  (** Double register operands *)
  | Mem_64_mem_64 : i64 register_ref * i64 register_ref -> [> `Mem_64_mem_64 ] operand
  (** Double memory operands *)
  | Reg_64_imm_a : i64 register * _ immediate -> [> `Reg_64_imm_a ] operand
  (** Mixed double operands *)
  | Imm_a : _ immediate -> [> `Imm_a ] operand (** Single immidiate operand *)
  | Reg_64 : i64 register -> [> `Reg_64 ] operand (** Single register operand*)
  | Mem_64 : i64 register_ref -> [> `Mem_64 ] operand (** Single memory operand*)
  | Nothing : [> `Nothing ] operand (** Mnemonic without operand (e.g Ret)*)
  | Label : string -> [> `Label ] operand (** Label operand (e.g Jmp lbl) *)
  | Reg_128 : i128 register -> [> `Reg_128 ] operand
  | Reg_64_reg_128 : i64 register * i128 register -> [> `Reg_64_reg_128 ] operand
  | Reg_128_reg_64 : i128 register * i64 register -> [> `Reg_128_reg_64 ] operand
  | Reg_128_reg_128 : i128 register * i128 register -> [> `Reg_128_reg_128 ] operand
  | Reg_128_reg_64_imm_a :
      i128 register * i64 register * 'a immediate
      -> [> `Reg_128_reg_64_imm_a ] operand
[@@deriving variants]

(** Some (actually most of them) mnemonic variants are not supported, but it's easy to add them later *)
type _ mnemonic =
  | Mov : [< `Reg_64_imm_a ] mnemonic
  | Push : [< `Reg_64 ] mnemonic
  | Add : [< `Reg_64_reg_64 ] mnemonic
  | Xor : [< `Reg_64_reg_64 ] mnemonic
  | Syscall : [< `Nothing ] mnemonic
  | Pop : [< `Reg_64 ] mnemonic
  | Jmp : [< `Label ] mnemonic
  | Sub : [< `Reg_64_imm_a ] mnemonic
  | Cmp : [< `Reg_64_imm_a ] mnemonic
  | Je : [< `Label ] mnemonic
  | Jne : [< `Label ] mnemonic
  | Jle : [< `Label ] mnemonic
  | Movq : [< `Reg_64_reg_128 | `Reg_128_reg_64 ] mnemonic
  | Punpckhqdq : [< `Reg_128_reg_128 ] mnemonic
  | Movapd : [< `Reg_128_reg_128 ] mnemonic
  (** Real instruction name, not the cat jumped on the keyboard *)
  | Pinsrq : [< `Reg_128_reg_64_imm_a ] mnemonic
  | Addpd : [< `Reg_128_reg_128 ] mnemonic
  | Mulpd : [< `Reg_128_reg_128 ] mnemonic
  | Haddpd : [< `Reg_128_reg_128 ] mnemonic
  (** Not sure that these instructions can really be used with integer values in xmm registers. The specification of the instructions is a complete mess*)
[@@deriving variants]

type directive =
  | Section of string
  | Global of string
[@@deriving variants]

type statement =
  | Directive of directive
  | Label_decl of string
  | Instruction : 'kind_of mnemonic * 'kind_of operand -> statement
[@@deriving variants]

type ast = statement list

let show_register = Variants_of_register.to_name
let show_mnemonic = Variants_of_mnemonic.to_name

let show_immediate = function
  | Imm_int x -> Printf.sprintf {|(Imm_int %d)|} x
;;

let show_register_ref = function
  | Register_ref x -> Printf.sprintf {|(Register_ref %s)|} (show_register x)
;;

let show_directive = function
  | Section x -> Printf.sprintf {|(Section %s)|} x
  | Global x -> Printf.sprintf {|(Global %s)|} x
;;

type args_wrapper = Wargs : 'kind_of operand -> args_wrapper [@@deriving variants]

type register_wrapper = Wregister : 'a register -> register_wrapper
[@@deriving variants]

let show_operand (Wargs op) =
  match op with
  | Reg_64 x -> Printf.sprintf {|(Reg_64 %s)|} (show_register x)
  | Mem_64 x -> Printf.sprintf {|(Mem_64 %s)|} (show_register_ref x)
  | Mem_64_mem_64 (x, y) ->
    Printf.sprintf
      {|(Mem_64_mem_64 (%s, %s))|}
      (show_register_ref x)
      (show_register_ref y)
  | Reg_64_reg_64 (x, y) ->
    Printf.sprintf {|(Reg_64_reg_64 (%s, %s))|} (show_register x) (show_register y)
  | Imm_a x -> Printf.sprintf {|(Imm_a %s)|} (show_immediate x)
  | Label x -> Printf.sprintf {|(Label %s)|} x
  | Nothing -> "(Nothing)"
  | Reg_64_imm_a (x, y) ->
    Printf.sprintf {|(Reg_64_imm_a (%s, %s))|} (show_register x) (show_immediate y)
  | Reg_128_reg_64 (x, y) ->
    Printf.sprintf {|(Reg_128_reg_64 (%s, %s))|} (show_register x) (show_register y)
  | Reg_128_reg_128 (x, y) ->
    Printf.sprintf {|(Reg_128_reg_128 (%s, %s))|} (show_register x) (show_register y)
  | Reg_64_reg_128 (x, y) ->
    Printf.sprintf {|(Reg_64_reg_128 (%s, %s))|} (show_register x) (show_register y)
  | Reg_128 x -> Printf.sprintf {|(Reg_128 %s)|} (show_register x)
  | Reg_128_reg_64_imm_a (x, y, z) ->
    Printf.sprintf
      {|(Reg_128_reg_64_imm_a (%s, %s, %s))|}
      (show_register x)
      (show_register y)
      (show_immediate z)
;;

let show_statement = function
  | Directive x -> Printf.sprintf {|(Directive %s)|} (show_directive x)
  | Label_decl x -> Printf.sprintf {|(LabelDecl %s)|} x
  | Instruction (mnemonic, args) ->
    Printf.sprintf
      {|(Instruction (%s %s))|}
      (show_mnemonic mnemonic)
      (show_operand (Wargs args))
;;

let show_ast ast = String.concat "\n" (List.map show_statement ast)
