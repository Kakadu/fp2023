(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type ast_type =
  | DeclarationList
  | MixedList
  | FreeExpression

val determine_ast_type : program -> ast_type
val is_keyword : string -> bool
val is_whitespace : char -> bool
val is_digit : char -> bool
val is_upper : char -> bool
val is_lower : char -> bool
val is_ident : char -> bool
val is_acceptable_fl : char option -> char Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val econst : Ast.const -> Ast.expr
val ebinop : Ast.bin_op -> Ast.expr -> Ast.expr -> Ast.expr
val eunop : Ast.un_op -> Ast.expr -> Ast.expr
val elist : Ast.expr list -> Ast.expr
val elistcons : Ast.expr -> Ast.expr -> Ast.expr
val etuple : Ast.expr list -> Ast.expr
val eidentifier : string -> Ast.expr
val eapplication : Ast.expr -> Ast.expr -> Ast.expr
val efun : Ast.pattern -> Ast.expr -> Ast.expr
val elet_in : string -> Ast.expr -> Ast.expr -> Ast.expr
val erec_let_in : string -> Ast.expr -> Ast.expr -> Ast.expr
val eif_then_else : Ast.expr -> Ast.expr -> Ast.expr -> Ast.expr
val ematch_with : Ast.expr -> (Ast.pattern * Ast.expr) list -> Ast.expr
val etry_with : Ast.expr -> Ast.effect_handler list -> Ast.expr
val eeffect_without_arguments : string -> Ast.expr
val eefect_with_arguments : string -> Ast.expr -> Ast.expr
val eeffect_perform : Ast.expr -> Ast.expr
val eeffect_continue : Ast.continue_val -> Ast.expr -> Ast.expr
val econt_val : string -> Ast.continue_val
val ddeclaration : string -> Ast.expr -> Ast.decl
val drec_declaration : string -> Ast.expr -> Ast.decl
val deffect_declaration : string -> Ast.effect_types_annotation -> Ast.decl
val sdeclaration : Ast.decl -> Ast.struct_item
val sexpression : Ast.expr -> Ast.struct_item
val sadd : 'a -> Ast.bin_op
val ssub : 'a -> Ast.bin_op
val smul : 'a -> Ast.bin_op
val sdiv : 'a -> Ast.bin_op
val seq : 'a -> Ast.bin_op
val sneq : 'a -> Ast.bin_op
val sgt : 'a -> Ast.bin_op
val sgte : 'a -> Ast.bin_op
val slt : 'a -> Ast.bin_op
val slte : 'a -> Ast.bin_op
val sand : 'a -> Ast.bin_op
val sor : 'a -> Ast.bin_op
val umin : 'a -> Ast.un_op
val unot : 'a -> Ast.un_op
val uplus : 'a -> Ast.un_op
val pany : 'a -> Ast.pattern
val pnill : 'a -> Ast.pattern
val pconst : Ast.const -> Ast.pattern
val pval : string -> Ast.pattern
val plist_cons : Ast.pattern -> Ast.pattern -> Ast.pattern
val ptuple : Ast.pattern list -> Ast.pattern
val peffect_without_args : string -> Ast.pattern
val peffect_with_args : string -> Ast.pattern -> Ast.pattern
val aint : Ast.effect_types_annotation
val abool : Ast.effect_types_annotation
val achar : Ast.effect_types_annotation
val astring : Ast.effect_types_annotation
val aunit : Ast.effect_types_annotation

val aarrow
  :  Ast.effect_types_annotation
  -> Ast.effect_types_annotation
  -> Ast.effect_types_annotation

val alist : Ast.effect_types_annotation -> Ast.effect_types_annotation
val atuple : Ast.effect_types_annotation list -> Ast.effect_types_annotation
val aeffect : Ast.effect_types_annotation -> Ast.effect_types_annotation
val effecthandler : Ast.pattern -> Ast.expr -> Ast.continue_val -> Ast.effect_handler
val continue : string -> Ast.continue_val
