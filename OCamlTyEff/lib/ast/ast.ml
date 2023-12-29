(** Copyright 2023 Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

type ident = Ident of string  (** Identifiers *)
[@@deriving show {with_path= false}, eq]

type constant =
  | Const_integer of int  (** Integer such as [25] *)
  | Const_char of char  (** Character such as ['c'] *)
  | Const_string of string
      (** Constant string such as ["constant"] or [{|other constant|}] *)
  | Const_float of float  (** Float such as [3.14] *)
[@@deriving show {with_path= false}]

(* ======= Patterns ======= *)

type pattern =
  | Pat_any  (** The pattern [_] *)
  | Pat_var of string  (** A variable pattern such as [x] *)
  | Pat_constant of constant
      (** Patterns such as [1], ['a'], ["hello"], [1.5] *)
  | Pat_tuple of pattern list
      (** Patterns [(P1, ..., Pn)]. Invariant: [n >= 2] *)
  | Pat_or of pattern * pattern  (** Pattern [P1 | P2] *)
  | Pat_construct of ident * pattern option
      (** [Pat_construct(C, args)] represents:
          - [C]   when [args] is [None]
          - [C P] when [args] is [Some P]
        *)
[@@deriving show {with_path= false}]

(* ======= Expressions ======= *)

type rec_flag =
  | Recursive  (** Recursive value binding *)
  | Nonrecursive  (** Nonrecursive value binding *)
[@@deriving show {with_path= false}]

and value_binding = {pat: pattern; expr: expression}
[@@deriving show {with_path= false}]

and function_body =
  | Function_body of expression  (** Expression as function body *)
  | Function_cases of case list  (** For functions defined using [function] *)
[@@deriving show {with_path= false}]

(** Pattern matching case *)
and case = {left: pattern; right: expression}
[@@deriving show {with_path= false}]

and expression =
  | Exp_ident of ident  (** Identifiers such as [x], [fact] *)
  | Exp_constant of constant
      (** Expression constant such as [1], ['a'], ["hello"], [1.5] *)
  | Exp_let of rec_flag * value_binding list * expression
      (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
          - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive] 
        *)
  | Exp_function of pattern list * function_body
      (** [Exp_function ([P1; ...; Pn], body)] represents any construct
          involving [fun] or [function], including:
          - [fun P1 ... Pn -> E]
              when [body = Function_body E]
          - [fun P1 ... Pn -> function p1 -> e1 | ... | pm -> em]
              when [body = Function_cases [ p1 -> e1; ...; pm -> em ]]

          A function must have parameters. [Exp_function (params, body)] must
          have non-empty [params] or a [Function_cases _] body.
        *)
  | Exp_apply of expression * expression
      (** [Exp_apply(E0, E1)] represents [E0 E1] *)
  | Exp_match of expression * case list
      (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Exp_tuple of expression list
      (** Expressions [(E1, ..., En)]. Invariant: [n >= 2] *)
  | Exp_construct of ident * expression option
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
  | Str_value of rec_flag * value_binding list
      (** [Str_value(rec, [(P1, E1) ; ... ; (Pn, En)])] represents:
          - [let P1 = E1 and ... and Pn = EN]
              when [rec] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN ]
              when [rec] is [Recursive]
        *)
[@@deriving show {with_path= false}]
