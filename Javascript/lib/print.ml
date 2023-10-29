(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Ast

let pp_ok ?(parser) str =
  Format.printf "%a" pp_statement @@ Result.get_ok(parse ?parser str)
;;

let pp_error str = match Result.get_error(parse str) with
  | `ParsingError s -> Format.eprintf "%s" s
;;