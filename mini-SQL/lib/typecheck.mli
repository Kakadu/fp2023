(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

module Exec : functor (M : Utils.MONAD_FAIL) -> sig
  val ( #+ ) : Types.item -> Types.item -> (Types.item, 'a) M.t
  val ( #- ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #* ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #/ ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #% ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #= ) : Types.item -> Types.item -> (bool, 'a) M.t
  val ( #> ) : Types.item -> Types.item -> (bool, 'a) M.t
  val ( #< ) : Types.item -> Types.item -> (bool, 'a) M.t
  val ( #!= ) : Types.item -> Types.item -> (bool, 'a) M.t
  val ( #>= ) : Types.item -> Types.item -> (bool, 'a) M.t
  val ( #<= ) : Types.item -> Types.item -> (bool, 'a) M.t
end
