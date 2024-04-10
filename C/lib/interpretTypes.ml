(** Copyright 2023-2024, PavlushaSource *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Stdint
open Ast

type value =
  | I_Int32 of int32
  | I_Int16 of int16
  | I_Int8 of int8
  | I_Char of char
  | I_Bool of bool
  | I_Null

type error =
  | CommandOutsideLoop of string
  | Unreachable
  | ReturnTypeMismatch of string
  | InvalidFunctionCall of string
  | NotImplemented of string
  | UnknownVariable of string
  | UnknownFunction of string
  | DivisionByZero
  | StackOverflow

let pp_value fmt = function
  | I_Int32 num ->
      fprintf fmt "%s" @@ Int32.to_string num
  | I_Int16 num ->
      fprintf fmt "%s" @@ Int16.to_string num
  | I_Int8 num ->
      fprintf fmt "%s" @@ Int8.to_string num
  | I_Char ch ->
      fprintf fmt "%c" ch
  | I_Bool bool ->
      fprintf fmt "%b" bool
  | I_Null ->
      fprintf fmt "Null value"

let pp_error fmt = function
  | UnknownFunction str ->
      fprintf fmt "Unknown function with name - %s" str
  | CommandOutsideLoop str ->
      fprintf fmt "%s command is out of the loop" str
  | InvalidFunctionCall str ->
      fprintf fmt "Invalid function call - %s" str
  | StackOverflow ->
      fprintf fmt "Stack overflow!"
  | Unreachable ->
      fprintf fmt "Unreachable"
  | ReturnTypeMismatch str ->
      fprintf fmt "Return type uncorrect - %s" str
  | NotImplemented str ->
      fprintf fmt "%s not implemented now" str
  | UnknownVariable str ->
      fprintf fmt "Unknown variable with name - %s" str
  | DivisionByZero ->
      fprintf fmt "Division by zero!"

module StringMap = Map.Make (String)

module type MONAD_ERROR = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t

  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type var_stack = {addr_in_stack: int; var_type: types}

type var_malloc = {own_heap: Bytes.t; var_type: types}

type variable = Stack_var of var_stack | Heap_var of var_malloc

type jmp_state = Break of bool | Return of bool | Continue of bool

type context =
  { return_type: types
  ; func_name: name
  ; stack: Bytes.t
  ; var_map: variable StringMap.t
  ; free_byte_stack: int
  ; functions_list: program
  ; last_value: value
  ; jump_state: jmp_state
  ; in_loop: bool }
