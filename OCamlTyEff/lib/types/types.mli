(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module Var : module type of Vars.Var
module VarSet : module type of Vars.VarSet

module Ty : module type of Tys.Ty
module Eff : module type of Tys.Eff
module Scheme : module type of Tys.Scheme

module Env : module type of Env
module StdEnv : module type of StdEnv
