(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val inference_expr : Ast.expr -> unit
val inference_program : Ast.struct_item list -> unit
val inference : string -> unit
val interpret_expr : Ast.expr -> unit
val interpret_program : Ast.struct_item list -> unit
val interpret : string -> unit
val parse_with_print : string -> unit
