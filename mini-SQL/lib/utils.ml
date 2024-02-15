(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

type error =
  | ReadError of string
  | UnknownTable of string
  | UnknownColumn of string
  | UnknownType of string
  | TypesMismatch of Types.item * string * Types.item
  | TypeConversionFail of Types.item * string
  | DivisionByZero
  | NotImplementedYet of string
  | IncorrectData of string

let error_to_string = function
  | ReadError msg -> Format.sprintf "Can't read data: '%s'" msg
  | UnknownTable msg -> Format.sprintf "Can't find table '%s'" msg
  | UnknownColumn msg -> Format.sprintf "Can't find column '%s'" msg
  | TypesMismatch (t1, op, t2) ->
    Format.sprintf
      "Type check failed: %s %s %s"
      (Types.show_item t1)
      op
      (Types.show_item t2)
  | TypeConversionFail (x, y) ->
    Format.sprintf "Failure to convert %s to type %s" (Types.show_item x) y
  | DivisionByZero -> "Division by zero encountered"
  | NotImplementedYet f ->
    Format.sprintf "You used a feature that has not yet been implemented yet (%s)" f
  | UnknownType t -> Format.sprintf "Can't recognize column type '%s'" t
  | IncorrectData f -> Format.sprintf "Element '%s' have incorrect structure" f
;;

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

(* RESULT MONADE *)
module Result : sig
  type ('a, 'e) t = ('a, 'e) Result.t

  include MONAD_FAIL with type ('a, 'e) t := ('a, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
end = struct
  include Base.Result

  type ('a, 'e) t = ('a, 'e) Result.t

  let fail msg = Error msg

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end
end
