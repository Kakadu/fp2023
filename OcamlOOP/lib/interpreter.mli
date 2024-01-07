(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Errors
open Base

type var
type enviroment

val eval
  :  string
  -> ( (string, Typedtree.scheme, String.comparator_witness) Map.t * enviroment
       , error )
       result

val eval_with_printing : string -> unit

module PP : sig
  open Format

  val pp_error : formatter -> interpreter_error -> unit
  val pp_eval : formatter -> string -> Typedtree.ty -> var -> unit

  val pp_program
    :  formatter
    -> (string, Typedtree.scheme, String.comparator_witness) Map.t
    -> enviroment
    -> unit
end
