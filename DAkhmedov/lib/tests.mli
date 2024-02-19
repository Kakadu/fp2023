(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests : sig
  val parse_test : string -> OCaml_ExtensibleVariantTypes_lib.Ast.decl list -> bool
end

module InferenceTests : sig
  val infer_test : string -> unit
end

module InterpretTests : sig
  val interpret_test : string -> unit
end
