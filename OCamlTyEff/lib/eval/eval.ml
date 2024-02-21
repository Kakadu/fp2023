(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module Val = Values.Val

module Bounds = Values.Bounds
module Env = Values.Env

module EvalMonad = Values.EvalMonad
module EvalError = Values.EvalError

let eval_structure_item ~printer env str_item =
  EvalMonad.run ~printer (Structure.eval str_item) env
  |> Result.map ~f:(fun ((value, idents), env) -> (env, idents, value))
