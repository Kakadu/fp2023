(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree
open Errors

val check_program
  :  structure_item list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

module PP : sig
  val pp_type : Format.formatter -> ty -> unit

  val pp_program
    :  Format.formatter
    -> (string, scheme, Base.String.comparator_witness) Base.Map.t
    -> unit

  val pp_error : Format.formatter -> infer_error -> unit
end
