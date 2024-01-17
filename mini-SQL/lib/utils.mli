(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

(** Interpretation error *)
type error =
  | ParsingError of string
  | ReadError of string
  | UnknownTable of string
  | UnknownColumn of string
  | UnknownType of string
  | TypesMismatch of Types.item * string * Types.item
  | TypeConversionFail of Types.item * string
  | DivisionByZero
  | NotImplementedYet of string
  | IncorrectData of string

val error_to_string : error -> string

(* monads ʕᵔᴥᵔʔ ʕᵔᴥᵔʔ ʕᵔᴥᵔʔ ʕᵔᴥᵔʔ ʕᵔᴥᵔʔ *)

module type MONAD_FAIL = sig
  type ('a, 'e) t

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val fail : 'e -> ('a, 'e) t
end

module Result : sig
  type ('a, 'e) t = ('a, 'e) result

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val fail : 'e -> ('a, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
end
