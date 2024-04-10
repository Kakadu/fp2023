(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

module T = struct
  type t = Ident of string
  [@@deriving eq, ord, sexp_of, show {with_path= false}]
end

include T
include Comparator.Make (T)
