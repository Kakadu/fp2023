open! Base

module T = struct
  type t = Var of string
  [@@deriving eq, ord, sexp_of, show {with_path= false}]
end

include T
include Comparator.Make (T)
