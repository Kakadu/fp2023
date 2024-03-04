(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : 'a Angstrom.t -> string -> ('a -> string) -> unit
val pp_infer : Ast.expr -> unit
val pp_parse_expr_and_infer : string -> unit
val pp_interpret_expr : Ast.expr -> unit
val pp_run : string -> unit
