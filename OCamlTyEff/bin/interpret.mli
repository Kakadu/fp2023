(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

type env = Types.Env.t * Eval.Env.t

val std_env : env

val interpret : term:LTerm.t -> ?rec_types:bool -> env -> string -> env Lwt.t
