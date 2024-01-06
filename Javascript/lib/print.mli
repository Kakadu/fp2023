(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_ok: ?parser:Ast.statement Angstrom.t -> string -> unit

val pp_error: string -> unit