open! Base
open Constraints

module Sub : module type of Sub

val solve : ConstrSet.t -> (Sub.t, TyError.t) result
