(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors

val parse : string -> (structure_item list, error) result
val parse_prefix : string -> (structure_item list, error) result

module PP : sig
  val pp_error : Format.formatter -> parse_error -> unit
end
