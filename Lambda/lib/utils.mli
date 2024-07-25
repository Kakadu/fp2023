[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val free_vars : string Ast.t -> string list
val is_free_in : string -> string Ast.t -> bool

(** Smart constructors *)

val var : 'a -> 'a Ast.t
val abs : 'a -> 'a Ast.t -> 'a Ast.t
val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
