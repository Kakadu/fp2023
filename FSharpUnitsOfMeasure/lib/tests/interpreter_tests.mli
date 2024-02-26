(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast
open Interpret_error
open Interpreter

val run_test : expression list -> (value, error) result
