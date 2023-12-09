open! Base
open Types

val unify : Ty.t -> Ty.t -> Sub.t SolveMonad.t
