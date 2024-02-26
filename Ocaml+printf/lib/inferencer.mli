(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Impossible_state of string
  | `Invalid_format_str of string
  | `Ivalid_format_concat of Typedtree.typ * Typedtree.typ
  | `Multiple_bound of string
  | `No_variable of string
  | `Occurs_check
  | `Unification_failed of Typedtree.typ * Typedtree.typ
  | `Unexpected_expr of Ast.expr
  ]

val pp_error : Format.formatter -> error -> unit

module TypeEnv : sig
  type t

  val std : t
  val find : t -> string -> Typedtree.scheme option
  val pp_env : Format.formatter -> t -> unit
end

val run_infer_expr : Ast.expr -> (Typedtree.typ * Ast.expr, error) result
val run_infer_program : Ast.program -> (TypeEnv.t * Ast.program, error) result
