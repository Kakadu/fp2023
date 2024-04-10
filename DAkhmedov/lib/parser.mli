(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_space : char -> bool
val parse_space : string Angstrom.t
val parse_space1 : string Angstrom.t
val parse_token : 'a Angstrom.t -> 'a Angstrom.t
val parse_token1 : 'a Angstrom.t -> 'a Angstrom.t
val parse_next_token : string Angstrom.t -> bool
val parse_stoken : string -> string Angstrom.t
val parse_stoken1 : string -> string Angstrom.t
val parse_next_stoken : string -> string Angstrom.t
val parse_parens : 'a Angstrom.t -> 'a Angstrom.t
val spaces : unit Angstrom.t
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_letter : char -> bool
val is_ident_char : char -> bool
val is_sign : char -> bool
val keywords : string list
val is_keyword : string -> bool
val parse_int : Ast.const Angstrom.t
val parse_bool : Ast.const Angstrom.t
val parse_const : Ast.const Angstrom.t
val parse_const_expr : Ast.expr Angstrom.t
val parse_satisfy_op_and_run : string -> 'a Angstrom.t -> 'a Angstrom.t
val parse_unary_op : Ast.unary_op Angstrom.t
val parse_tuple : 'a Angstrom.t -> ('a list -> 'a) -> 'a Angstrom.t
val parse_tuple_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_tuple_pat : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_branching : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val binop_binder : string -> Ast.bin_op -> (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val add : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val sub : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val mul : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val div : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val geq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val gre : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val leq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val less : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val eq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val neq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val and_l : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val or_l : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val app_binder : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_un_op_app : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_bin_op_app : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_list : 'a Angstrom.t -> ('a -> 'b -> 'b) -> 'b -> 'b Angstrom.t
val parse_list_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_list_pat : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_letters : string Angstrom.t
val parse_identifier : string Angstrom.t
val parse_identifier_expr : Ast.expr Angstrom.t
val parse_pattern : Ast.pattern Angstrom.t
val parse_pattern_with_list : Ast.pattern Angstrom.t
val parse_params : Ast.pattern list Angstrom.t
val parse_fun : string -> Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_fun_decl : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_fun_anon : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_closure : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_fun_anon_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_match : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_expr : Ast.expr Angstrom.t
val postprocess_expr : Ast.expr -> Ast.expr
val parse_expr : Ast.expr Angstrom.t
val parse_decl_let : Ast.expr Angstrom.t -> Ast.decl Angstrom.t
val parse_decls : Ast.prog Angstrom.t
val parse_program : string -> (Ast.prog, string) result
