(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Pp
open Common

let%expect_test "parse_bool1" =
  pp Format.pp_print_string parse_constr_name "true" ;
  [%expect {| true |}]

let%expect_test "parse_bool2" =
  pp Format.pp_print_string parse_constr_name "false" ;
  [%expect {| false |}]

let%expect_test "parse_unit" =
  pp Format.pp_print_string parse_constr_name "()" ;
  [%expect {| () |}]

let%expect_test "parse_quoted_string_literal1" =
  pp pp_constant parse_const "{|Hello world!|}" ;
  [%expect {| (Const_string "Hello world!") |}]

let%expect_test "parse_quoted_string_literal2" =
  pp pp_constant parse_const "{aa|Hello world!|aa}" ;
  [%expect {| (Const_string "Hello world!") |}]

let%expect_test "parse_quoted_string_literal3" =
  pp pp_constant parse_const "{aa|Hello |aa world!|aa}" ;
  [%expect {| (Const_string "Hello |aa world!") |}]

let%expect_test "parse_value_name1" =
  pp Format.pp_print_string parse_value_name "abc" ;
  [%expect {| abc |}]

let%expect_test "parse_value_name2" =
  pp Format.pp_print_string parse_value_name "a0b'c_d" ;
  [%expect {| a0b'c_d |}]

let%expect_test "parse_value_name3" =
  pp Format.pp_print_string parse_value_name "_0a" ;
  [%expect {| _0a |}]

let%expect_test "parse_value_name_operator1" =
  pp Format.pp_print_string parse_value_name "(>>)" ;
  [%expect {| >> |}]

let%expect_test "parse_value_name_operator2" =
  pp Format.pp_print_string parse_value_name "(%>)" ;
  [%expect {| %> |}]

let%expect_test "parse_value_name_operator3" =
  pp Format.pp_print_string parse_value_name "(!)" ;
  [%expect {| ! |}]

let%expect_test "parse_value_name_operator4" =
  pp Format.pp_print_string parse_value_name "(~:=)" ;
  [%expect {| ~:= |}]

let%expect_test "parse_value_name_operator5" =
  pp Format.pp_print_string parse_value_name "(@<>)" ;
  [%expect {| @<> |}]

let%expect_test "parse_constr_name" =
  pp Format.pp_print_string parse_constr_name "Nil" ;
  [%expect {| Nil |}]
