(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type const =
  | Int of int (** Integer *)
  | Bool of bool (** Boolean *)
  | Empty (** () *)

val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type pattern =
  | PAny (**  _ *)
  | PConst of const (** const *)
  | PVar of id (** string *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type bin_op =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Mul (** [*] *)
  | Div (** [/] *)
  | Eq (** [=] *)
  | Neq (** [!=] *)
  | Les (** [<] *)
  | Leq (** [<=] *)
  | Gre (** [>] *)
  | Geq (** [>=] *)

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

type expression =
  | Var of id (**string *)
  | Const of const (** const *)
  | IfThenElse of expression * expression * expression (** if ... then ... else ... *)
  | Binop of bin_op * expression * expression (** 1 + 2 *)
  | Fun of id * expression (** fun *)
  | List of expression list (** [1;2;3] *)
  | Match of expression * (pattern * expression) list (** match *)
  | App of expression * expression (** f x *)
  | Let of fun_rec * id * expression * expression option (** let *)

and fun_rec =
  | Rec (** let rec f*)
  | NoRec (** let f *)

val pp_expression : Format.formatter -> expression -> unit
val pp_fun_rec : Format.formatter -> fun_rec -> unit
val show_expression : expression -> string
val show_fun_rec : fun_rec -> string

type expr_list = expression list

val pp_expr_list : Format.formatter -> expr_list -> unit
val show_expr_list : expr_list -> string
