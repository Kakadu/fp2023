(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_banned : string -> bool
val is_digit : char -> bool
val is_space : char -> bool
val is_sign : char -> bool
val is_valid_func_first_char : char -> bool
val is_eol : char -> bool
val is_whitespace : char -> bool
val is_stmt_sep : char -> bool
val is_variable : char -> bool
val is_quotes : char -> bool
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val skip_whitespace : unit Angstrom.t
val between : 'a Angstrom.t -> 'b Angstrom.t -> 'c Angstrom.t -> 'c Angstrom.t
val round_brackets : 'a Angstrom.t -> 'a Angstrom.t
val skip_stmt_sep : unit Angstrom.t
val take_number : string Angstrom.t
val take_string : string Angstrom.t
val take_variable : string Angstrom.t
val take_sign : string Angstrom.t
val token : string -> string Angstrom.t
val t_return : string Angstrom.t
val t_def : string Angstrom.t
val t_mul : string Angstrom.t
val t_eq : string Angstrom.t
val t_not_eq : string Angstrom.t
val t_quote : string Angstrom.t
val t_div : string Angstrom.t
val t_assign : string Angstrom.t
val t_mod : string Angstrom.t
val t_comma : string Angstrom.t
val t_sub : string Angstrom.t
val t_add : string Angstrom.t
val t_if : string Angstrom.t
val t_else : string Angstrom.t
val t_while : string Angstrom.t
val t_and : string Angstrom.t
val t_greater : string Angstrom.t
val t_less_equal : string Angstrom.t
val t_greater_equal : string Angstrom.t
val t_less : string Angstrom.t
val t_or : string Angstrom.t
val exp_add : Ast.expression -> Ast.expression -> Ast.expression
val exp_sub : Ast.expression -> Ast.expression -> Ast.expression
val exp_mul : Ast.expression -> Ast.expression -> Ast.expression
val exp_div : Ast.expression -> Ast.expression -> Ast.expression
val exp_mod : Ast.expression -> Ast.expression -> Ast.expression
val exp_eq : Ast.expression -> Ast.expression -> Ast.expression
val exp_not_eq : Ast.expression -> Ast.expression -> Ast.expression
val stmt_expression : Ast.expression -> Ast.statement

val stmt_func
  :  Ast.identifier
  -> Ast.identifier list
  -> Ast.statement list
  -> Ast.statement

val stmt_if_else
  :  Ast.expression
  -> Ast.statement list
  -> Ast.statement list
  -> Ast.statement

val stmt_return : Ast.expression -> Ast.statement
val stmt_assign : Ast.expression -> Ast.expression -> Ast.statement
val stmt_while : Ast.expression -> Ast.statement list -> Ast.statement
val exp_func_call : Ast.identifier -> Ast.expression list -> Ast.expression
val exp_and : Ast.expression -> Ast.expression -> Ast.expression
val exp_or : Ast.expression -> Ast.expression -> Ast.expression
val exp_greater : Ast.expression -> Ast.expression -> Ast.expression
val exp_less : Ast.expression -> Ast.expression -> Ast.expression
val exp_greater_equal : Ast.expression -> Ast.expression -> Ast.expression
val exp_less_equal : Ast.expression -> Ast.expression -> Ast.expression

val lift_func_call
  :  Ast.identifier Angstrom.t
  -> Ast.expression list Angstrom.t
  -> Ast.expression Angstrom.t

val lift_return : Ast.expression Angstrom.t -> Ast.statement Angstrom.t

val lift_func
  :  Ast.identifier Angstrom.t
  -> Ast.identifier list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val lift_assign
  :  Ast.expression Angstrom.t
  -> Ast.expression Angstrom.t
  -> Ast.statement Angstrom.t

val lift_if_else
  :  Ast.expression Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val lift_expression : Ast.expression Angstrom.t -> Ast.statement Angstrom.t

val lift_while
  :  Ast.expression Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val p_mul : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_div : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_mod : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_sub : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_add : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_assign : Ast.expression Angstrom.t -> Ast.statement Angstrom.t
val p_eq : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_not_eq : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_and : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_or : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_greater : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_gr_eq : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_less : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_less_eq : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val p_integer : Ast.expression Angstrom.t
val p_string : Ast.expression Angstrom.t
val p_variable : Ast.expression Angstrom.t

val p_if_else
  :  Ast.expression Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val p_identifier : Ast.identifier Angstrom.t
val p_func_call : Ast.expression list Angstrom.t -> Ast.expression Angstrom.t
val p_return : Ast.expression Angstrom.t -> Ast.statement Angstrom.t

val p_func
  :  Ast.identifier list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val p_while
  :  Ast.expression Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val p_lambda : 'a -> 'b -> 'c Angstrom.t
val mp_high_pr_op : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val mp_low_pr_op : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val gp_comparison_ops : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t
val gp_logic_ops : (Ast.expression -> Ast.expression -> Ast.expression) Angstrom.t

type dispatch =
  { p_expression : dispatch -> Ast.expression Angstrom.t
  ; p_statement : dispatch -> Ast.statement Angstrom.t
  }

val p_exp_or_stmt : dispatch
val parse : 'a Angstrom.t -> string -> ('a, string) result
val pyParser : Ast.statement list Angstrom.t
val parser : string -> (Ast.statement list, string) result
