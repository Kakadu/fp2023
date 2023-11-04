(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Wrappers *)
val eq_wrap : eq:('a -> 'b -> bool) -> 'b -> 'a option -> bool
val show_wrap : (Format.formatter -> 'a -> unit) -> 'a option -> unit

(* Parsers for test *)
val test_pars : 'a Angstrom.t -> ('a -> 'b -> bool) -> string -> 'b -> bool
val print_pars : 'a Angstrom.t -> (Format.formatter -> 'a -> unit) -> string -> unit
