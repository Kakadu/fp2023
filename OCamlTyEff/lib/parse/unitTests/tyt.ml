(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Pp

let parse_ty = Ty.parse_ty

open Types

let%expect_test "parse_var" =
  pp Ty.pp parse_ty "'some_type_var" ;
  [%expect {| 'some_type_var |}]

let%expect_test "parse_arrow" =
  pp Ty.pp parse_ty "'a -> 'b -> 'c" ;
  [%expect {| 'a -> 'b -> 'c |}]

let%expect_test "parse_tuple" =
  pp Ty.pp parse_ty "'a * 'b * 'c" ;
  [%expect {| 'a * 'b * 'c |}]

let%expect_test "parse_constr1" = pp Ty.pp parse_ty "int" ; [%expect {| int |}]

let%expect_test "parse_constr2" =
  pp Ty.pp parse_ty "int list" ;
  [%expect {| int list |}]

let%expect_test "parse_constr3" =
  pp Ty.pp parse_ty "(int, string) map" ;
  [%expect {|
    (int, string) map |}]

let%expect_test "parse_constr4" =
  pp Ty.pp parse_ty "('a -> int * (string, unit, 'b -> 'c) foo bar) -> e" ;
  [%expect {|
    ('a -> int * (string, unit, ('b -> 'c)) foo bar) -> e |}]
