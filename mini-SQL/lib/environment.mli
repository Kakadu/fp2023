(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

module Env : functor (M : Utils.MONAD_FAIL) -> sig
  val load_database : string -> (Types.Database.t, Utils.error) M.t
end
