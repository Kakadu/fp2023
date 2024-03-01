(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

val const_value : Ast.value -> Ast.expr
val var : Ast.modifier -> Ast.id -> Ast.expr
val plus : Ast.expr -> Ast.expr -> Ast.expr
val minus : Ast.expr -> Ast.expr -> Ast.expr
val equal : Ast.expr -> Ast.expr -> Ast.expr
val notEqual : Ast.expr -> Ast.expr -> Ast.expr
val lessOrEqual : Ast.expr -> Ast.expr -> Ast.expr
val less : Ast.expr -> Ast.expr -> Ast.expr
val greaterOrEqual : Ast.expr -> Ast.expr -> Ast.expr
val greater : Ast.expr -> Ast.expr -> Ast.expr
val mult : Ast.expr -> Ast.expr -> Ast.expr
val div : Ast.expr -> Ast.expr -> Ast.expr
val funcMonoCall : Ast.id -> Ast.expr list -> Ast.expr
val funcPolyCall : Ast.id -> Ast.id -> Ast.expr list -> Ast.expr
val listAccess : Ast.id -> Ast.expr -> Ast.expr
val modOp : Ast.expr -> Ast.expr -> Ast.expr
val andOp : Ast.expr -> Ast.expr -> Ast.expr
val orOp : Ast.expr -> Ast.expr -> Ast.expr
val callLambda : Ast.id list -> Ast.statement list -> Ast.expr list -> Ast.expr
val assign : Ast.expr -> Ast.expr -> Ast.statement
val multiAssign : Ast.expr list -> Ast.expr list -> Ast.statement
val expr : Ast.expr -> Ast.statement
val func : Ast.id -> Ast.id list -> Ast.statement list -> Ast.statement
val ifElse : Ast.expr -> Ast.statement list -> Ast.statement list -> Ast.statement
val returns : Ast.expr -> Ast.statement
val continue : Ast.statement
val break : Ast.statement
val puts : Ast.expr -> Ast.statement
val whileLoop : Ast.expr -> Ast.statement list -> Ast.statement
val classDef : Ast.id -> Ast.statement list -> Ast.statement
val id : string -> Ast.id
val int : int -> Ast.value
val float : float -> Ast.value
val bool : bool -> Ast.value
val str : string -> Ast.value
val null : Ast.expr
val obj : Ast.id -> Ast.value
val lambda : Ast.id list -> Ast.statement list -> Ast.expr
val listExpr : Ast.expr list -> Ast.expr
val is_whitespace : char -> bool
val is_seporator : char -> bool
val is_allowed_first_letter : char -> bool
val is_digit : char -> bool
val is_signed : char -> bool
val is_dot : char -> bool
val is_quote : char -> bool
val is_variable : char -> bool
val is_reserved : string -> bool
val skip_whitespace : unit Angstrom.t
val skip_seporator : unit Angstrom.t
val between : 'a Angstrom.t -> 'b Angstrom.t -> 'c Angstrom.t -> 'c Angstrom.t
val round_brackets : 'a Angstrom.t -> 'a Angstrom.t
val curly_brackets : 'a Angstrom.t -> 'a Angstrom.t
val square_brackets : 'a Angstrom.t -> 'a Angstrom.t
val vertical_bars : 'a Angstrom.t -> 'a Angstrom.t
val take_number : string Angstrom.t
val take_sign : string Angstrom.t
val take_dot : string Angstrom.t
val take_string : string Angstrom.t
val take_variable : string Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val plus_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val minus_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val mult_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val div_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val mod_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val equal_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val less_or_equal_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val greater_or_equal_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val less_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val greater_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val notequal_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val and_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val or_parse : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val null_parse : Ast.expr Angstrom.t
val break_parse : Ast.statement Angstrom.t
val continue_parse : Ast.statement Angstrom.t
val true_parse : Ast.expr Angstrom.t
val false_parse : Ast.expr Angstrom.t
val id_parse : Ast.id Angstrom.t
val local_var_parse : Ast.expr Angstrom.t
val instance_var_parse : Ast.expr Angstrom.t
val global_var_parse : Ast.expr Angstrom.t
val class_var_parse : Ast.expr Angstrom.t
val integer_parse : Ast.expr Angstrom.t
val float_parse : Ast.expr Angstrom.t
val string_parse : Ast.expr Angstrom.t
val obj_parse : Ast.expr Angstrom.t
val func_mono_cal_parse : Ast.expr list Angstrom.t -> Ast.expr Angstrom.t
val func_poly_call_parse : Ast.expr list Angstrom.t -> Ast.expr Angstrom.t
val list_access_parse : Ast.id Angstrom.t -> Ast.expr Angstrom.t -> Ast.expr Angstrom.t

val func_parse
  :  Ast.id list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val if_else_parse
  :  Ast.expr Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val return_parse : Ast.expr Angstrom.t -> Ast.statement Angstrom.t
val puts_parse : Ast.expr Angstrom.t -> Ast.statement Angstrom.t
val class_parse : Ast.statement list Angstrom.t -> Ast.statement Angstrom.t

val while_parse
  :  Ast.expr Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.statement Angstrom.t

val lambda_parse
  :  Ast.id list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.expr Angstrom.t

val call_lambda_parse
  :  Ast.id list Angstrom.t
  -> Ast.statement list Angstrom.t
  -> Ast.expr list Angstrom.t
  -> Ast.expr Angstrom.t

val assign_parse : Ast.expr Angstrom.t -> Ast.statement Angstrom.t
val multiple_assign_parse : Ast.expr list Angstrom.t -> Ast.statement Angstrom.t
val expression_parse : Ast.statement Angstrom.t -> Ast.expr Angstrom.t
val statement_parse : Ast.statement Angstrom.t
val final_parse : Ast.statement list Angstrom.t
val parse : 'a Angstrom.t -> string -> ('a, string) result
val parser_result_to_stmt_list : string -> Ast.statement list
val interpret_parse : 'a Angstrom.t -> ('a -> string) -> string -> unit
