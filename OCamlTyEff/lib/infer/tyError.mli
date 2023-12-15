open! Base
open Types
open Ast

type t =
  | UnificationMismatch  (** Lists of types to unify have different lengths *)
  | UnificationFail of Ty.t * Ty.t  (** Failed to unify two types *)
  | UnboundVariable of Ident.t
      (** Failed to find a variable in the environment *)
  | OccursIn of Var.t * Ty.t  (** Type variable occurs in a type *)
  | PatVarBoundSeveralTimes of Ident.t
      (** Pattern(s) bound the same variable several times. E.g. `let x, x = ..` *)
  | ConstructorArityMismatch of Ident.t
      (** Constructor is not applied (`Some`), or applied when not needed (`None 1`) *)
  | NotVarLHSRec
      (**
        The left hand side of the recursive binding is not a var.
        E.g. `let rec _ = ..`
      *)
  | NotImplemented of string
      (**
        Feature is not yet implemented in the type checker.
        The argument contains the feature description
      *)

val pp : Format.formatter -> t -> unit
