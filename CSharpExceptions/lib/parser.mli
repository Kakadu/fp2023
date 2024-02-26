(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** {2 Modifier parsers} *)

open Angstrom

val p_access_modifier : Ast.access_modifier t
val p_method_modifier : Ast.method_modifier t
val p_fild_modifier : Ast.fild_modifier t

(** {2 Assignable type parsers} *)

val ep_number : Ast.expr t
val ep_char : Ast.expr t
val ep_string : Ast.expr t
val ep_bool : Ast.expr t
val ep_identifier : Ast.expr t
val ep_value : Ast.expr t

(** {2 Language constructs parsers} *)

val ep_member_ident : Ast.expr t
val ep_method_member : Ast.class_member t
val ep_var_decl : Ast.var_decl t
val ep_operation : Ast.expr t
val ep_assign : Ast.expr t
val ep_method_invoke : Ast.expr t
val ep_decl : Ast.statement t
val ep_break : Ast.statement t
val ep_return : Ast.statement t
val ep_steps : Ast.statement t
val ep_brunch_loop : Ast.statement t
val ep_try_catch_fin : Ast.statement t

(** {2 Main parsers} *)

val ep_class_members : Ast.class_member list t
val ep_class : Ast.class_decl t
val ep_classes : Ast.tast t

(** [parse s] - this parser will read the string s and return the result *)
val parse_ast : string -> (Ast.tast, string) result

val parse_until_true : 'a t -> string -> ('a, string) result
val parse_option : string -> p:'a t -> 'a option
