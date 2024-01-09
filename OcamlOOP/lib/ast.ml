(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string [@@deriving show { with_path = false }]

type private_flag =
  | Private
  | Public
[@@deriving show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Asterisk (** [*] *)
  | Divider (** [\ ]*)
  | Plus (** [+] *)
  | Sub (** [-] *)
  | Eq (** [=] *)
  | Neq (** [!=]*)
  | Lt (** [<] *)
  | Ltq (** [<=]*)
  | Gt (** [>] *)
  | Gtq (** [>=]*)
  | And (** [&&]*)
  | Or (** [||]*)
[@@deriving show { with_path = false }]

type unary_op =
  | Minus (** [-]*)
  | Not (** [not]*)
[@@deriving show { with_path = false }]

type const =
  | Const_int of int (** Integers constants such as [52] *)
  | Const_bool of bool (** Boolean constant: [true], [false]*)
  | Const_nil (** Represents empty list [[]] *)
[@@deriving show { with_path = false }]

type pattern =
  | Pat_const of const (** Patterns such as [1], [true] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_cons of pattern * pattern (** The pattern such as [P1::P2] *)
  | Pat_any (** The pattern [_] *)
  | Pat_tuple of pattern list (** Patterns [(P1, ..., P2)]
                                  Invariant : [n >= 2]*)
[@@deriving show { with_path = false }]

type expression =
  | Exp_constant of const (** Expressions constant such as [1], [true] *)
  | Exp_unary_op of unary_op * expression
  | Exp_bin_op of bin_op * expression * expression
  | Exp_ident of ident (** Identifiers such as [x] *)
  | Exp_tuple of expression list (** Expressions [(E1, ..., En)]
                                     Invariant: [n >= 2] *)
  | Exp_function of pattern * expression (** [fun P1 -> E] *)
  | Exp_let of decl * expression
  (** [Exp_let({d_rec=flag; P; E}, E')] represents:
      - [let P = E in E'] when [flag] is {{!rec_flag.Nonrecursive} [Nonrecursive]}
      - [let rec P = E in E'] when [d_rec] is {{!rec_flag.Recursive} [Recursive]} *)
  | Exp_match of expression * (pattern * expression) list
  (** [match E0 with P1 -> E1 | .. | Pn -> En] *)
  | Exp_ifthenelse of expression * expression * expression (** [if E1 then E2 else E3] *)
  | Exp_apply of expression * expression (** [E0 E1] *)
  | Exp_object of obj (** [object ... end]*)
  | Exp_send of expression * ident (** [E # m]*)
  (*  | Exp_list of expression list (** [[E1; ..; En]]*)*)
  | Exp_override of (ident * expression) list (** [{< x1 = E1; ...; xn = En >}] *)
  | Exp_list of expression * expression
  (** The expression such as [E1::E2]
      This also represents lists [E1; ... En] via [E]*)
[@@deriving show { with_path = false }]

(** Represents:
    - [let P = E] when [d_rec] is {{!rec_flag.Nonrecursive} [Nonrecursive]}
    - [let rec P = E] when [d_rec] is {{!rec_flag.Recursive} [Recursive]} *)
and decl =
  { d_rec : rec_flag
  ; d_pat : pattern
  ; d_expr : expression
  }

(** Values of type {!obj} represents:
    - [object(selfpat) ... end]
    - [object ... end] when {{!obj.o_self} [o_self] is {{!pattern.Pat_any} [Pat_any]}} *)
and obj =
  { o_self : pattern
  ; o_fields : obj_field list
  }

and obj_field =
  | Obj_val of ident * expression (** [val x = E] *)
  | Obj_method of private_flag * ident * expression (** [method x = E]*)

type structure_item =
  | Str_eval of expression (** [E] *)
  | Str_value of decl
  (** [Str_value({is_rec; P; E})] represents:
      - [let P = E] when {{!rec_flag.Nonrecursive}[Nonrecursive]
      - [let rec P = E] when [d_rec] is {{!rec_flag.Recursive}[Recursive]} *)
[@@deriving show { with_path = false }]

(** Represents whole program with all statements *)
type program = structure_item list [@@deriving show { with_path = false }]
