(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** {2 Modifier parsers} *)

val p_access_modifier : Ast.access_modifier Angstrom.t
val p_method_modifier : Ast.method_modifier Angstrom.t
val p_fild_modifier : Ast.fild_modifier Angstrom.t

(** {2 Assignable type parsers} *)

val ep_number : Ast.expr Angstrom.t
val ep_char : Ast.expr Angstrom.t
val ep_string : Ast.expr Angstrom.t
val ep_bool : Ast.expr Angstrom.t
val ep_identifier : Ast.expr Angstrom.t
val ep_value : Ast.expr Angstrom.t

(** {2 Language constructs parsers} *)

val ep_member_ident : Ast.expr Angstrom.t
val ep_var_decl : Ast.expr Angstrom.t
val ep_operation : Ast.expr Angstrom.t
val ep_assign : Ast.expr Angstrom.t
val ep_method_invoke : Ast.expr Angstrom.t
val ep_eAssign_eDecl : Ast.expr Angstrom.t
val ep_break : Ast.expr Angstrom.t
val ep_return : Ast.expr Angstrom.t
val ep_steps : Ast.expr Angstrom.t
val ep_brunch_loop : Ast.expr Angstrom.t

(** {2 Main parsers} *)

val ep_class_members : Ast.class_member list Angstrom.t
val ep_class : Ast.class_sign Angstrom.t
val ep_classes : Ast.tast Angstrom.t

(** [parse s] - this parser will read the string s and return the result *)
val parse : string -> (Ast.tast, string) result
