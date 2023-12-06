open! Base
open Types

val unify : Ty.t -> Ty.t -> Substitution.t SolveMonad.t
