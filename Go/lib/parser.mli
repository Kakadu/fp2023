(** Copyright 2021-2023, Nikita Lukonenko and Furetur *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_space : char -> bool
val spaces : string Angstrom.t
val space1 : string Angstrom.t
val varname : char Angstrom.t
val conde : 'a Angstrom.t list -> 'a Angstrom.t

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit
val string_of_char : char -> string
val str_char : string Angstrom.t
val str_body : string Angstrom.t
val str_const : Ast.constant Angstrom.t
val is_digit : char -> bool
val parse_int : Ast.constant Angstrom.t
val parse_int_not_const : int Angstrom.t
val word_parser : string Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val braces : 'a Angstrom.t -> 'a Angstrom.t
val keyword : string -> string Angstrom.t
val parse1 : 'a Angstrom.t -> string -> ('a, string) result
val ident : Ast.expression Angstrom.t
val spend_char : char -> unit Angstrom.t
val comma_separated : 'a Angstrom.t -> 'a list Angstrom.t
val type_name : Ast.type' Angstrom.t
val parse_typ : Ast.ret_typ Angstrom.t
val parse_option_res : Ast.ret_typ Angstrom.t
val arg_decl : (string * Ast.type') Angstrom.t
val parse_args : (string * Ast.type') list Angstrom.t
val signature : Ast.signature Angstrom.t

type expr_declar_dispatch =
  { expression : expr_declar_dispatch -> Ast.expression Angstrom.t
  ; top_level_declaration : expr_declar_dispatch -> Ast.top_level_declaration Angstrom.t
  ; func_declaration : expr_declar_dispatch -> Ast.func_declaration Angstrom.t
  ; block : expr_declar_dispatch -> Ast.block Angstrom.t
  ; statements : expr_declar_dispatch -> Ast.statements Angstrom.t
  }

val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t

type helper = Args of Ast.expression list

val ex_decl_disp : expr_declar_dispatch
val expression : Ast.expression Angstrom.t
val func_decl : Ast.func_declaration Angstrom.t
val top_level_decl : Ast.top_level_declaration Angstrom.t
val block : Ast.block Angstrom.t
val stmt : Ast.statements Angstrom.t
