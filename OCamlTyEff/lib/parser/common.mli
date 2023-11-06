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
  [let P1 = E1 and P2 = E2 and ...]
  [let rec ValName1 PArg1 = E1 and P1 = E2 and ...]
*)

(**
  Helpers needed to parse expressions with prefix operators

  Binding power of the prefix operator is its precedence (expressed as positive integer)
  relative to other operators (both prefix and infix).
  Higher number means higher precedence
*)
type ('oprnd, 'op) prefix_helpers =
  {parse: 'op t; get_binding_power: 'op -> int; apply: 'op -> 'oprnd -> 'oprnd}

type 'op infix_operator = {op: 'op; op_length: int}

(**
  Helpers needed to parse expressions with infix operators

  Binding power of the infix operator sets its precedence and associativity
  relative to other operators (both prefix and infix)

  Binding power is two positive integers representing left and right bps of infix operators.
  If left bp is higher than right bp then operator is right-associative.
  If right bp is higher than left bp then operator is left-associative.
  Higher numbers overall set higher precedence relative to other operators

  E.g. infix left-associative (10, 11)
  has higher precedence than infix right-associative (8, 7)
  which has higher precedence than prefix with bp of 5

  {!fold} is used to fold expressions with infix ops
  into one bigger expression of type 'oprnd.
  The first argument is accumulator
  (initialized with left-hand side of the first operator expression to fold).
  The second argument is (operator * right-hand side of corresponding operator expression)
*)
type ('oprnd, 'op) infix_helpers =
  { peek: 'op infix_operator t
  ; get_binding_power: 'op -> int * int
  ; fold: 'oprnd -> 'op * 'oprnd -> 'oprnd }

val parse_operators :
     ?prefix:('oprnd, 'prefix_op) prefix_helpers
  -> ?infix:('oprnd, 'infix_op) infix_helpers
  -> 'oprnd t
  -> 'oprnd t
(**
  Parse expressions with infix and prefix operators.
  See {!prefix_helpers} and {!infix_helpers} documentation for more information
*)
