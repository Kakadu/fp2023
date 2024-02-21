(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Eval

(** Built-in function *)
type builtin = Val.t -> Val.t EvalMonad.t

val id : string -> Ident.t

val sc : string -> Scheme.t
(** Parse string to type *)

val var : string -> Ty.t
(** Get type variable with provided name *)

val alpha : Ty.t
(** 'a type variable *)

val ( @> ) : Ty.t * Eff.t -> Ty.t -> Ty.t
(** Construct arrow type *)

val no_vars : Ty.t -> Scheme.t
(** Construct scheme without quantified type variables *)

val single_var : Ty.t -> Scheme.t
(** Construct scheme with single quantified variable - 'a *)

val eff : Eff.Label.t -> Eff.t
(** Construct effect with single label *)

val extract_int : Val.t -> int EvalMonad.t
(** Extract int from value *)

val extract_string : Val.t -> string EvalMonad.t
(** Extract string from value *)

val make_2args_fun : (Val.t -> Val.t -> Val.t EvalMonad.t) -> builtin
(** Convert function of 2 args to built-in *)

val make_bin_fun_int : (int -> int -> int) -> builtin
(** Make built-in from binary int function *)

val make_bin_fun_bool : (int -> int -> bool) -> builtin
(** Make built-in from binary bool function *)
