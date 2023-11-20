(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Common
open Typ

let%expect_test "parse_var" =
  pp pp_typ parse_typ "'some_type_var" ;
  [%expect {| (Typ_var "some_type_var") |}]

let%expect_test "parse_arrow" =
  pp pp_typ parse_typ "'a -> 'b -> 'c" ;
  [%expect
    {| (Typ_arrow ((Typ_var "a"), (Typ_arrow ((Typ_var "b"), (Typ_var "c"))))) |}]

let%expect_test "parse_tuple" =
  pp pp_typ parse_typ "'a * 'b * 'c" ;
  [%expect {| (Typ_tuple [(Typ_var "a"); (Typ_var "b"); (Typ_var "c")]) |}]

let%expect_test "parse_constr1" =
  pp pp_typ parse_typ "int" ; [%expect {| (Typ_constr ((Ident "int"), [])) |}]

let%expect_test "parse_constr2" =
  pp pp_typ parse_typ "int list" ;
  [%expect
    {| (Typ_constr ((Ident "list"), [(Typ_constr ((Ident "int"), []))])) |}]

let%expect_test "parse_constr3" =
  pp pp_typ parse_typ "(int, string) map" ;
  [%expect
    {|
    (Typ_constr ((Ident "map"),
       [(Typ_constr ((Ident "int"), [])); (Typ_constr ((Ident "string"), []))])) |}]

let%expect_test "parse_constr4" =
  pp pp_typ parse_typ "('a -> int * (string, unit, 'b -> 'c) foo bar) -> e" ;
  [%expect
    {|
    (Typ_arrow (
       (Typ_arrow ((Typ_var "a"),
          (Typ_tuple
             [(Typ_constr ((Ident "int"), []));
               (Typ_constr ((Ident "bar"),
                  [(Typ_constr ((Ident "foo"),
                      [(Typ_constr ((Ident "string"), []));
                        (Typ_constr ((Ident "unit"), []));
                        (Typ_arrow ((Typ_var "b"), (Typ_var "c")))]
                      ))
                    ]
                  ))
               ])
          )),
       (Typ_constr ((Ident "e"), [])))) |}]
