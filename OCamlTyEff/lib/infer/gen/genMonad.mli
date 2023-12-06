open! Base
open Monads.Std
open Types
open Constraints

include Monad.S

val run : 'a t -> 'a * ConstrSet.t

module Gen : sig
  val varset : VarSet.t t

  val extend_varset : Var.t list -> 'a t -> 'a t

  val add_constrs : Constr.t list -> unit t

  val fresh_var : Var.t t
end
