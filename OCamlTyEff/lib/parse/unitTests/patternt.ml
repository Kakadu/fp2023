(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Pp
open Pattern

let%expect_test "parse_var" =
  pp pp_pattern parse_pattern "a" ;
  [%expect {| (Pat_var (Ident "a")) |}]

let%expect_test "parse_any" =
  pp pp_pattern parse_pattern "_" ;
  [%expect {| Pat_any |}]

let%expect_test "parse_const" =
  pp pp_pattern parse_pattern "5" ;
  [%expect {| (Pat_constant (Const_integer 5)) |}]

let%expect_test "parse_constr1" =
  pp pp_pattern parse_pattern "C" ;
  [%expect {| (Pat_construct ((Ident "C"), None)) |}]

let%expect_test "parse_constr2" =
  pp pp_pattern parse_pattern "C a" ;
  [%expect {| (Pat_construct ((Ident "C"), (Some (Pat_var (Ident "a"))))) |}]

let%expect_test "parse_constr3" =
  pp pp_pattern parse_pattern "Cons (hd, tl)" ;
  [%expect
    {|
    (Pat_construct ((Ident "Cons"),
       (Some (Pat_tuple [(Pat_var (Ident "hd")); (Pat_var (Ident "tl"))])))) |}]

let%expect_test "parse_constr_or_tuple" =
  pp pp_pattern parse_pattern "C _ | a, b" ;
  [%expect
    {|
    (Pat_or ((Pat_construct ((Ident "C"), (Some Pat_any))),
       (Pat_tuple [(Pat_var (Ident "a")); (Pat_var (Ident "b"))]))) |}]

let%expect_test "parse_or" =
  pp pp_pattern parse_pattern "a | (b | c) | d" ;
  [%expect
    {|
    (Pat_or (
       (Pat_or ((Pat_var (Ident "a")),
          (Pat_or ((Pat_var (Ident "b")), (Pat_var (Ident "c")))))),
       (Pat_var (Ident "d")))) |}]

let%expect_test "parse_tuple" =
  pp pp_pattern parse_pattern "a, (b, c), d" ;
  [%expect
    {|
    (Pat_tuple
       [(Pat_var (Ident "a"));
         (Pat_tuple [(Pat_var (Ident "b")); (Pat_var (Ident "c"))]);
         (Pat_var (Ident "d"))]) |}]

let%expect_test "parse_or_tuple" =
  pp pp_pattern parse_pattern "a, b | c, d" ;
  [%expect
    {|
    (Pat_or ((Pat_tuple [(Pat_var (Ident "a")); (Pat_var (Ident "b"))]),
       (Pat_tuple [(Pat_var (Ident "c")); (Pat_var (Ident "d"))]))) |}]

let%expect_test "parse_list_op" =
  pp pp_pattern parse_pattern "a::(b::c)::d" ;
  [%expect
    {|
    (Pat_construct ((Ident "::"),
       (Some (Pat_tuple
                [(Pat_var (Ident "a"));
                  (Pat_construct ((Ident "::"),
                     (Some (Pat_tuple
                              [(Pat_construct ((Ident "::"),
                                  (Some (Pat_tuple
                                           [(Pat_var (Ident "b"));
                                             (Pat_var (Ident "c"))]))
                                  ));
                                (Pat_var (Ident "d"))]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_list_or_tuple" =
  pp pp_pattern parse_pattern "a::b::c,d|e" ;
  [%expect
    {|
    (Pat_or (
       (Pat_tuple
          [(Pat_construct ((Ident "::"),
              (Some (Pat_tuple
                       [(Pat_var (Ident "a"));
                         (Pat_construct ((Ident "::"),
                            (Some (Pat_tuple
                                     [(Pat_var (Ident "b"));
                                       (Pat_var (Ident "c"))]))
                            ))
                         ]))
              ));
            (Pat_var (Ident "d"))]),
       (Pat_var (Ident "e")))) |}]

let%expect_test "parse_list" =
  pp pp_pattern parse_pattern "[a;b;c]" ;
  [%expect
    {|
    (Pat_construct ((Ident "::"),
       (Some (Pat_tuple
                [(Pat_var (Ident "a"));
                  (Pat_construct ((Ident "::"),
                     (Some (Pat_tuple
                              [(Pat_var (Ident "b"));
                                (Pat_construct ((Ident "::"),
                                   (Some (Pat_tuple
                                            [(Pat_var (Ident "c"));
                                              (Pat_construct ((Ident "[]"), None
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_list_1element" =
  pp pp_pattern parse_pattern "[a]" ;
  [%expect
    {|
    (Pat_construct ((Ident "::"),
       (Some (Pat_tuple
                [(Pat_var (Ident "a")); (Pat_construct ((Ident "[]"), None))]))
       )) |}]

let%expect_test "parse_list_empty" =
  pp pp_pattern parse_pattern "[]" ;
  [%expect {| (Pat_construct ((Ident "[]"), None)) |}]
