(** Copyright 2021-2023, Ilya Syresenkov, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing

module TypeEnv : sig
  type t = (id, scheme, Base.String.comparator_witness) Base.Map.t
end

val run_infer : expr -> (ty, error) result
val typecheck : TypeEnv.t -> program -> (TypeEnv.t, error) result
