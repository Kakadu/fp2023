(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Interpret_error

type environment = (id, value, String.comparator_witness) Map.t

val interpreter
  :  ?environment:(string, value, String.comparator_witness) Map.t
  -> expression list
  -> (environment * value, error) result
