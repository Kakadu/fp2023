(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

module Exec : functor (M : Utils.MONAD_FAIL) -> sig
  val ( #+ ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #- ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #* ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #/ ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #% ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #= ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #> ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #< ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #!= ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #>= ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #<= ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val not : Types.item -> (Types.item, Utils.error) M.t
  val ( #&& ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t
  val ( #|| ) : Types.item -> Types.item -> (Types.item, Utils.error) M.t

  (** Converts item to bool or fail *)
  val bool_of_item : Types.item -> (bool, Utils.error) M.t
end
