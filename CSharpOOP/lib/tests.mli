(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val show_wrap : (Format.formatter -> 'a -> unit) -> 'a option -> unit
val print_pars : 'a Angstrom.t -> (Format.formatter -> 'a -> unit) -> string -> unit
