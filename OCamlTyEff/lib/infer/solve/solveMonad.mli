open! Base
open Monads.Std
open Types

include Monad.S

val run : 'a t -> ('a, TyError.t) result

module Solve : sig
  val fresh_var : Var.t t

  val fail : TyError.t -> 'a t
end
