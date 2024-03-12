(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VInt of int (* 1 *)
  | VBool of bool (* true *)
  | VChar of char (* 'c' *)
  | VString of string (* "hello" *)
  | VUnit (* () *)
  | VTuple of value list (* (1,'c', bool) *)
  | VList of value list (* [1; 3; 4] or (1 :: 2 :: 3 :: [4]) *)
  | VFun of pattern * expr * enviroment (* fun x -> expr (x) *)
  | VRecFun of id * value (* let rec f = fun x -> expr (x, f) *)
  | VEffectDeclaration of id (* effect DevisionByZero : int -> int effect *)
  | VEffectWithArguments of id * value (* DevisionByZero x *)
  | VEffectWithoutArguments of id (* DevisionByZero *)
  | VEffectContinue of
      continue_val (* continue k 0 - here k is the continuation variable *)
  | VThrowingValue of value (* continue k 0 - here 0 is the throwing value *)

(* The environment contains the values ​​of let bindings and declared effects. *)
(* Example: let f = 5 will be presented as (f, VInt 5) *)
and enviroment = (string, value, Base.String.comparator_witness) Base.Map.t

(* Handlers contain effect handlers for effects that may occur in a given trywith block. *)
type handlers =
  (string, pattern * expr * continue_val, Base.String.comparator_witness) Base.Map.t

(* Constructors for values *)

let vint c = VInt c
let vbool c = VBool c
let vchar c = VChar c
let vstring c = VString c
let vunit = VUnit
let vtuple l = VTuple l
let vlist l = VList l
let vfun pat exp env = VFun (pat, exp, env)
let vrecfun ident v = VRecFun (ident, v)
let veffect_declaration n = VEffectDeclaration n
let veffect_with_arguments n a = VEffectWithArguments (n, a)
let veffect_without_arguments n = VEffectWithoutArguments n
let veffect_continue k = VEffectContinue k
let vthrowing_value v = VThrowingValue v

(* ---------------- *)
