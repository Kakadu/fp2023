(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module T = struct
  type t = Var of string [@@deriving eq, ord, sexp_of]

  let pp ppf (Var name) = Stdlib.Format.fprintf ppf "'%s" name
end

include T
include Comparator.Make (T)
