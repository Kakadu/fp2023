(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

(** Tests for AST & parser *)
module Parse_test : sig
  (** Run SQL-Parser on string *)
  val run : 'a Angstrom.t -> string -> ('a, string) result

  (** Compare parser output with example *)
  val assert_equal : 'a Angstrom.t -> string -> 'a -> bool
end

(** Tests for Types module *)
module Types_test : sig
  (** Compare parser output with example *)
  val assert_equal : 'a -> 'a -> ('a -> string) -> bool
end

module Typecheck_test : sig
  (** Compare op result with example *)
  val assert_equal
    :  'a
    -> ('a -> 'b -> (Types.item, Utils.error) result)
    -> 'b
    -> Types.item
    -> bool

  (** Returns true if error *)
  val assert_raise : 'a -> ('a -> 'b -> ('c, 'd) result) -> 'b -> bool
end
