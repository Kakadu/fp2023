(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val interpret
  :  Ast.expression list
  -> ( (string, Interpreter.value, Base.String.comparator_witness) Base.Map.t
       * Interpreter.value list
       , Interpreter.error )
       result

val extract_expressions : ('a * 'b) list -> 'a list
val process_program : string -> unit
