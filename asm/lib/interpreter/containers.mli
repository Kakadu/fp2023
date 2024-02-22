(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StringMap : sig
  include Map.S with type key = string
end

module IntMap : sig
  include Map.S with type key = int
end

module ListStack : sig
  type 'a t = 'a list

  val empty : 'a list
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a option
  val pop : 'a list -> 'a list option
end
