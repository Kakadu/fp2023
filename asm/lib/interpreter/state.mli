(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StateErrorMonad : sig
  type ('st, 'a) t = 'st -> 'st * ('a, string) result

  val return : 'a -> 'b -> 'b * ('a, 'c) result
  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val read : ('st, 'st) t
  val write : 'st -> ('st, unit) t
  val run : ('st, 'a) t -> 'st -> 'st * ('a, string) result
  val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
end

module GlobalState : sig
  type t =
    { registers : int Containers.StringMap.t
    ; xmm_registers : (int * int) Containers.StringMap.t
    ; stack : int list
    ; label_to_jmp : string option
    ; zero_flag : int
    ; carry_flag : int
    ; exit_code : int option
    }

  val initialize_registers_with : (string -> bool) -> 'a -> 'a Containers.StringMap.t
  val initialize_registers : int Containers.StringMap.t
  val initialize_xmm_registers : (int * int) Containers.StringMap.t
  val initial_state : t
  val update : ('a -> ('a, 'a) StateErrorMonad.t) -> ('a, unit) StateErrorMonad.t
  val show : t -> string
end

val read_label_to_jmp : (GlobalState.t, string option) StateErrorMonad.t
val read_zero_flag : (GlobalState.t, int) StateErrorMonad.t
val read_carry_flag : (GlobalState.t, int) StateErrorMonad.t
val peek_reg : (GlobalState.t, int option) StateErrorMonad.t

val read_generic_reg
  :  'a Ast.register
  -> ('b -> 'c Containers.StringMap.t)
  -> ('b, 'c) StateErrorMonad.t

val read_reg : 'a Ast.register -> (GlobalState.t, int) StateErrorMonad.t
val read_xmm_reg : 'a Ast.register -> (GlobalState.t, int * int) StateErrorMonad.t
val write_reg : 'a Ast.register -> int -> (GlobalState.t, unit) StateErrorMonad.t

val write_xmm_reg
  :  'a Ast.register
  -> int * int
  -> (GlobalState.t, unit) StateErrorMonad.t

val push_reg : int -> (GlobalState.t, unit) StateErrorMonad.t
val pop_to_reg : 'a Ast.register -> (GlobalState.t, unit) StateErrorMonad.t
val reset_label_to_jmp : (GlobalState.t, unit) StateErrorMonad.t
val write_label_to_jmp : string -> (GlobalState.t, unit) StateErrorMonad.t
val write_zero_flag : int -> (GlobalState.t, unit) StateErrorMonad.t
val write_carry_flag : int -> (GlobalState.t, unit) StateErrorMonad.t
val write_exit_code : int -> (GlobalState.t, unit) StateErrorMonad.t
