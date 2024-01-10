(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
module Ident = Ident

type constant =
  | Const_integer of int  (** Integer such as [25] *)
  | Const_char of char  (** Character such as ['c'] *)
  | Const_string of string
      (** Constant string such as ["constant"] or [{|other constant|}] *)
[@@deriving show {with_path= false}]

(* ======= Patterns ======= *)
and pattern =
  | Pat_any  (** The pattern [_] *)
  | Pat_var of Ident.t  (** A variable pattern such as [x] *)
  | Pat_constant of constant
      (** Patterns such as [1], ['a'], ["hello"], [1.5] *)
  | Pat_tuple of pattern list
      (** Patterns [(P1, ..., Pn)]. Invariant: [n >= 2] *)
  | Pat_or of pattern * pattern  (** Pattern [P1 | P2] *)
  | Pat_construct of Ident.t * pattern option
      (** [Pat_construct(C, args)] represents:
          - [C]   when [args] is [None]
          - [C P] when [args] is [Some P]
        *)
[@@deriving show {with_path= false}]

(* ======= Expressions ======= *)
and rec_flag =
  | Recursive  (** Recursive value binding *)
  | Nonrecursive  (** Nonrecursive value binding *)
[@@deriving show {with_path= false}]

and value_binding = {pat: pattern; expr: expression}
[@@deriving show {with_path= false}]

(** Pattern matching case *)
and case = {left: pattern; right: expression}
[@@deriving show {with_path= false}]

and expression =
  | Exp_ident of Ident.t  (** Identifiers such as [x], [fact] *)
  | Exp_constant of constant
      (** Expression constant such as [1], ['a'], ["hello"], [1.5] *)
  | Exp_let of rec_flag * value_binding list * expression
      (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
          - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive].
          Invariant: [n >= 1] 
        *)
  | Exp_fun of pattern list * expression
      (** [Exp_fun ([P1; ...; Pn], E)] represents [fun P1 ... Pn -> E].
          Invariant: [n >= 1]
        *)
  | Exp_function of case list
      (** [Exp_function([C1; ...; Cn])] represents [function C1 | ... | Cn].
          Invariant: [n >= 1] 
        *)
  | Exp_apply of expression * expression
      (** [Exp_apply(E0, E1)] represents [E0 E1] *)
  | Exp_match of expression * case list
      (** [match E0 with P1 -> E1 | ... | Pn -> En]. Invariant: [n >= 1] *)
  | Exp_try of expression * case list
      (** [try E0 with P1 -> E1 | ... | Pn -> En]. Invariant: [n >= 1] *)
  | Exp_tuple of expression list
      (** Expressions [(E1, ..., En)]. Invariant: [n >= 2] *)
  | Exp_construct of Ident.t * expression option
      (** [Exp_construct(C, exp)] represents:
          - [C]               when [exp] is [None],
          - [C E]             when [exp] is [Some E],
          - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])]
        *)
  | Exp_ifthenelse of expression * expression * expression option
      (** [if E1 then E2 else E3] *)
  | Exp_sequence of expression * expression  (** [E1; E2] *)
[@@deriving show {with_path= false}]

(* ======= Module structure ======= *)

type structure = structure_item list [@@deriving show {with_path= false}]

and structure_item =
  | Str_eval of expression  (** [E] *)
  | Str_exception of Ident.t  (** [exception Some_exc] *)
  | Str_value of rec_flag * value_binding list
      (** [Str_value(flag, [(P1, E1) ; ... ; (Pn, En)])] represents:
          - [let P1 = E1 and ... and Pn = EN]      when [flag] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN ] when [flag] is [Recursive].
          Invariant: [n >= 1]
        *)
[@@deriving show {with_path= false}]
