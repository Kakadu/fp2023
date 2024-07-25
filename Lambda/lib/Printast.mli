[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Pretty-print representation of AST with names. *)
val pp_named : Format.formatter -> string Ast.t -> unit

val pp : (Format.formatter -> 'name -> unit) -> Format.formatter -> 'name Ast.t -> unit
val show : (Format.formatter -> 'name -> unit) -> 'name Ast.t -> string
