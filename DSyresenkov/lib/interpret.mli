(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VTuple of value * value * value list
  | VList of value list
  | VFun of id * expr * bindings

and bindings = (id, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type interpret_result =
  { id : id option
  ; value : value
  ; ty : Typing.ty
  }

val pp_interpret_result : Format.formatter -> interpret_result -> unit

type error =
  | DivisionByZero
  | UnboundValue of id
  | IncorrectType of value
  | NotAFunction of value
  | LetWithoutIn (** Inner let expressions without in are forbidden *)
  | NoMatchCase of value
  | NotImplemented

val pp_error : Format.formatter -> error -> unit

val interpret
  :  (expr * Typing.ty) list
  -> (bindings * interpret_result list, error) Result.t
