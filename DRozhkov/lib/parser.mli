(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_space : char -> bool
val is_whitespace : char -> bool
val is_digit : char -> bool
val digit_c : char Angstrom.t
val l_low : char -> bool
val l_up : char -> bool
val variable : char -> bool
val letter : char -> bool
val spaces : unit Angstrom.t
val skip_whitespace : string Angstrom.t
val skip_whitespace1 : string Angstrom.t
val token : string -> string Angstrom.t
val ptoken : 'a Angstrom.t -> 'a Angstrom.t
val lp : string Angstrom.t
val rp : string Angstrom.t
val add : Ast.bin_op Angstrom.t
val sub : Ast.bin_op Angstrom.t
val mul : Ast.bin_op Angstrom.t
val div : Ast.bin_op Angstrom.t
val les : Ast.bin_op Angstrom.t
val leq : Ast.bin_op Angstrom.t
val gre : Ast.bin_op Angstrom.t
val geq : Ast.bin_op Angstrom.t
val comparison : Ast.bin_op Angstrom.t
val high_pr_op : Ast.bin_op Angstrom.t
val low_pr_op : Ast.bin_op Angstrom.t
val peq : Ast.bin_op Angstrom.t
val pneq : Ast.bin_op Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val is_syntax : string -> bool
val if_then_else : Ast.expression Angstrom.t -> Ast.expression Angstrom.t
val sign : string Angstrom.t
val dot : bool Angstrom.t
val number : Ast.expression Angstrom.t
val bool_pars : Ast.expression Angstrom.t
val ident : string Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val let_pars : Ast.expression Angstrom.t -> Ast.expression Angstrom.t
val list_pars : Ast.expression Angstrom.t -> Ast.expression Angstrom.t

val pebinop
  :  ('a -> (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t -> 'b)
  -> 'a
  -> Ast.bin_op Angstrom.t
  -> 'b

val plbinop
  :  Ast.expression Angstrom.t
  -> Ast.bin_op Angstrom.t
  -> Ast.expression Angstrom.t

val var_pars : Ast.expression Angstrom.t
val const_pars : Ast.expression Angstrom.t
val pnumber : Ast.pattern Angstrom.t
val pbool_pars : Ast.pattern Angstrom.t
val ppconst : Ast.pattern Angstrom.t
val ppvar : Ast.pattern Angstrom.t
val ppattern : Ast.pattern Angstrom.t
val ematch : Ast.expression Angstrom.t -> Ast.expression Angstrom.t
val pexpr : Ast.expression Angstrom.t
val parse : string -> (Ast.expression list, string) result
