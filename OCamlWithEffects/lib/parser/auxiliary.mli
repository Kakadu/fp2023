(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type ast_type =
  | DeclarationList
  | MixedList
  | FreeExpression

val determine_ast_type : expr list -> ast_type
val is_keyword : string -> bool
val is_whitespace : char -> bool
val is_digit : char -> bool
val is_upper : char -> bool
val is_lower : char -> bool
val is_ident : char -> bool
val is_acceptable_fl : char option -> char Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val econst : const -> expr
val ebinop : bin_op -> expr -> expr -> expr
val eunop : un_op -> expr -> expr
val elist : expr list -> expr
val elistcons : expr -> expr -> expr
val etuple : expr list -> expr
val eidentifier : string -> expr
val eapplication : expr -> expr -> expr
val efun : pattern -> expr -> expr
val edeclaration : string -> expr -> expr option -> expr
val erec_declaration : string -> expr -> expr option -> expr
val eif_then_else : expr -> expr -> expr -> expr
val ematch_with : expr -> (pattern * expr) list -> expr
val etry_with : expr -> effect_handler list -> expr
val eeffect_without_arguments : string -> expr
val eefect_with_arguments : string -> expr -> expr
val eeffect_declaration : string -> effect_types_annotation -> expr
val eeffect_perform : expr -> expr
val eeffect_continue : continue_val -> expr -> expr
val econt_val : string -> continue_val
val sadd : 'a -> bin_op
val ssub : 'a -> bin_op
val smul : 'a -> bin_op
val sdiv : 'a -> bin_op
val seq : 'a -> bin_op
val sneq : 'a -> bin_op
val sgt : 'a -> bin_op
val sgte : 'a -> bin_op
val slt : 'a -> bin_op
val slte : 'a -> bin_op
val sand : 'a -> bin_op
val sor : 'a -> bin_op
val umin : 'a -> un_op
val unot : 'a -> un_op
val uplus : 'a -> un_op
val pany : 'a -> pattern
val pnill : 'a -> pattern
val pconst : const -> pattern
val pval : string -> pattern
val plist_cons : pattern -> pattern -> pattern
val ptuple : pattern list -> pattern
val peffect_without_args : string -> pattern
val peffect_with_args : string -> pattern -> pattern
val aint : effect_types_annotation
val abool : effect_types_annotation
val achar : effect_types_annotation
val astring : effect_types_annotation
val aunit : effect_types_annotation
val aarrow : effect_types_annotation -> effect_types_annotation -> effect_types_annotation
val alist : effect_types_annotation -> effect_types_annotation
val atuple : effect_types_annotation list -> effect_types_annotation
val aeffect : effect_types_annotation -> effect_types_annotation
val effecthandler : pattern -> expr -> continue_val -> effect_handler
val continue : string -> continue_val
