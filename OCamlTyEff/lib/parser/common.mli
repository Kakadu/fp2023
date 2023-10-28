(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> string -> unit
(**
  Run parser on string and pretty print the output using printer.
  Used for inline expect tests
*)

val ws : unit t
(** skip whitespaces *)

val is_core_operator_char : char -> bool
(** core-operator-char ::= \{$ | & | * | + | - | / | = | > | @ | ^ | \| \} *)

val is_operator_char : char -> bool
(** operator-char ::= \{~ | ! | ? | core-operator-char | % | < | : | .\} *)

val peek_custom_infix_operator_name : string t
(** 
  infix-symbol ::= (core-operator-char | % | <) \{ operator-char \} 
*)

val parse_custom_prefix_operator_name : string t
(**
  prefix-symbol ::= ! \{ operator-char \} 
                    | (? | ~) \{ operator-char \}+
*)

val parse_value_name : string t
(**
  value-name ::= (a..z | _) \{ A..Z | a..z | 0..9 | _ | ' \}
                 | (prefix-symbol ∣ infix-symbol) 
  must not be keyword
*)

val parse_constr_name : string t
(** constr-name ::= (A..Z) \{ A..Z | a..z | 0..9 | _ | ' \} *)

val parse_const : constant t

val parse_let_binding :
  expression t -> pattern t -> (rec_flag * value_binding list) t
(**
  [let P1 = E1 and P2 = E2 and ... and Pn = En]
  [let rec P1 PArg1 = E1 and P2 = E2 and ... and Pn = En]
*)

type 'a infix_operator = {op: 'a; op_length: int}

val parse_infix_prefix :
     parse_operand:'exp t
  -> peek_infix_op:'infix infix_operator t
  -> get_infix_binding_power:('infix -> int * int)
  -> infix_fold_fun:('exp -> 'infix * 'exp -> 'exp)
  -> parse_prefix_op:'prefix t
  -> get_prefix_binding_power:('prefix -> int)
  -> apply_prefix_op:('prefix -> 'exp -> 'exp)
  -> 'exp t
(** Parse expressions with infix and prefix operators using Pratt parsing *)
