(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Ast

type t =
  | UnificationMismatch
  | UnificationFailTy of Ty.t * Ty.t
  | UnificationFailEff of Eff.t * Eff.t
  | UnboundVariable of Ident.t
  | OccursInTy of Var.t * Ty.t
  | OccursInEff of Var.t * Eff.t
  | RecursiveEffRows
  | PatVarBoundSeveralTimes of Ident.t
  | VarsMismatchOrPattern of Ident.t
  | ConstructorArityMismatch of Ident.t
  | NotVarLHSRec
  | NotAllowedRHSRec of expression
  | UnboundTypeVariable of string
  | UnboundType of Ident.t
  | TypeArityMismatch of Ident.t
  | NotImplemented of string
[@@deriving show {with_path= false}]
