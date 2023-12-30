(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

val pp_type : Format.formatter -> ty -> unit

val pp_program : Format.formatter -> (string * scheme) list -> unit

val pp_error : Format.formatter -> error -> unit

val pp_subst : Format.formatter -> (binder, ty, 'a) Base.Map.t -> unit

val pp_scheme : Format.formatter -> scheme -> unit

val pp_env : Format.formatter -> (string * scheme) list -> unit