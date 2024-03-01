(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type local_flag =
  | Local
  | Nonlocal
[@@deriving show { with_path = false }]

type ident = string [@@deriving show { with_path = false }]

type op_id =
  | Op_add (** E1 + E2 *)
  | Op_mul (** E1 * E2 *)
  | Op_div (** E1 / E2 *)
  | Op_sub (** E1 - E2 *)
  | Op_mod (** E1 % E2 *)
  | Op_concat (** E1 .. E2 *)
  | Op_eq (** E1 == E2 *)
  | Op_neq (** E1 ~= E2 *)
  | Op_lt (** E1 < E2 *)
  | Op_le (** E1 <= E2 *)
  | Op_and (** E1 and E2 *)
  | Op_or (** E1 or E2 *)
[@@deriving show { with_path = false }]

type block = statement list

and statement =
  | Stat_do of block (** [ do ... end ] *)
  | Stat_assign of local_flag * ident * expression (** [ [local] Id = E ] *)
  | Stat_while of expression * block (** [ while E do B end ] *)
  | Stat_if of (expression * block) list * block option
  (** [ if E1 then B1 [elseif E2 then B2] ...  [else BN] end ] *)
  | Stat_return of expression list (** [ return E1, E2 ... ] *)
  | Stat_break (** [ break ] *)
  | Stat_call of apply (** single fucntion call, like print(1) *)
[@@deriving show { with_path = false }]

and apply = Call of expression * expression list (** [ E1(E2, ... En) ] *)

and expression =
  | Exp_nil (** [ nil ] *)
  | Exp_true (** [ true ] *)
  | Exp_false (** [ false ] *)
  | Exp_number of float (** [ 42 ] *)
  | Exp_string of string (** [ "lua" ] *)
  | Exp_op of op_id * expression * expression (** [ E1 op E2 ]  particulary *)
  | Exp_function of ident list * block (** [ function (Id1,... Idn) ... end ] *)
  | Exp_call of apply (** function call in expr, like n + fact(n-1) *)
  | Exp_lhs of ident (* name of smth *)
[@@deriving show { with_path = false }]
