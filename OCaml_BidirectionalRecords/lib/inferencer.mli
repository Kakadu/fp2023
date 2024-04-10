(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val run_inference : Ast.expr -> (Typing.typ, Typing.error) result

module PP : sig
  val pp_type : Format.formatter -> Typing.typ -> unit
  val pp_error : Format.formatter -> Typing.error -> unit
  val print_typ : Typing.typ -> unit
  val print_type_error : Typing.error -> unit
  val print_result : Ast.expr -> unit
end
