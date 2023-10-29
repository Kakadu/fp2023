(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*
val var : 'a -> 'a Ast.t
val abs : 'a -> 'a Ast.t -> 'a Ast.t
val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t *)

val subst : string -> by:string Ast.t -> string Ast.t -> string Ast.t

type strat =
  { on_var : strat -> Ast.name -> string Ast.t
  ; on_abs : strat -> Ast.name -> string Ast.t -> string Ast.t
  ; on_app : strat -> string Ast.t -> string Ast.t -> string Ast.t
  }

val apply_strat : strat -> string Ast.t -> string Ast.t
val without_strat : strat

(** Predefined strategies *)

val cbn_strat : strat
val nor_strat : strat
val cbv_strat : strat
val ao_strat : strat

(** Example lambda expressions *)

val a : string Ast.t
val x : string Ast.t
val y : string Ast.t
val z : string Ast.t
val f : string Ast.t
val g : string Ast.t
val h : string Ast.t
val m : string Ast.t
val n : string Ast.t
val p : string Ast.t
val zero : string Ast.t
val one : string Ast.t
val two : string Ast.t
val three : string Ast.t
