(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val type_checker
  :  Ast.objects list
  -> State_type.TypeCheck.st_type_check * (unit, Monad.error) result

val type_checker_with_main
  :  Ast.objects list
  -> Ast.name option * (unit, Monad.error) result
