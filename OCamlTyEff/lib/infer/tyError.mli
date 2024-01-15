(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Ast

type t =
  | UnificationMismatch  (** Lists of types to unify have different lengths *)
  | UnificationFailTy of Ty.t * Ty.t  (** Failed to unify two types *)
  | UnificationFailEff of Eff.t * Eff.t  (** Failed to unify two effects *)
  | UnboundVariable of Ident.t
      (** Failed to find a variable in the environment *)
  | OccursInTy of Var.t * Ty.t  (** Type variable occurs in a type *)
  | OccursInEff of Var.t * Eff.t  (** Effect variable occurs in an effect *)
  | RecursiveEffRows  (** Recursive effect rows *)
  | PatVarBoundSeveralTimes of Ident.t
      (** Pattern(s) bound the same variable several times. E.g. `let x, x = ..` *)
  | ConstructorArityMismatch of Ident.t
      (** Constructor is not applied (`Some`), or applied when not needed (`None 1`) *)
  | NotVarLHSRec
      (**
        The left hand side of the recursive binding is not a var.
        E.g. `let rec _ = ..`
      *)
  | NotAllowedRHSRec of expression
      (**
        The expression is not allowed as right-hand side of `let rec'.
        E.g. `let rec x = x + 1`
      *)
  | NotImplemented of string
      (**
        Feature is not yet implemented in the type checker.
        The argument contains the feature description
      *)

val pp : Format.formatter -> t -> unit
