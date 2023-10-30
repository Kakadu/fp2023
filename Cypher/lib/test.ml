(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Base
open Cypher_lib.Ast
open Cypher_lib.Parser

let parse_and_print s = Stdlib.Format.printf "%a" pp_expression (parse_expr s)

let%expect_test "Simple expr" =
  parse_and_print {| 120.607 + 400 / 10 ^ 5 |};
  [%expect
    {|
    (Bin_op (Plus, (Const (Float 120.607)),
       (Bin_op (Slash, (Const (Int64 400L)),
          (Bin_op (Caret, (Const (Int64 10L)), (Const (Int64 5L))))))
       )) |}]
;;

let%expect_test "Simple expr with unary minus" =
  parse_and_print {|- 120.607 + -400 / 10 ^ -x |};
  [%expect
    {|
    (Bin_op (Plus, (Un_op (Minus, (Const (Float 120.607)))),
       (Bin_op (Slash, (Const (Int64 -400L)),
          (Bin_op (Caret, (Const (Int64 10L)), (Un_op (Minus, (Var "x")))))))
       )) |}]
;;

let%expect_test "Multiple sequential comparison operators" =
  parse_and_print {| 1+-1 = 0 <> 10 >= 5*1 <= 5/1 > - -4 |};
  [%expect
    {|
    (List_op ((Bin_op (Plus, (Const (Int64 1L)), (Const (Int64 -1L)))),
       [(Eq, (Const (Int64 0L))); (NEq, (Const (Int64 10L)));
         (GEq, (Bin_op (Asterisk, (Const (Int64 5L)), (Const (Int64 1L)))));
         (LEq, (Bin_op (Slash, (Const (Int64 5L)), (Const (Int64 1L)))));
         (Greater, (Un_op (Minus, (Const (Int64 -4L)))))]
       )) |}]
;;

let%expect_test "Null check operators" =
  parse_and_print {| 4 = (4 + a is null IS NOT NULL) is null |};
  [%expect
    {|
    (List_op ((Const (Int64 4L)),
       [(Eq,
         (Un_op (IS_NULL,
            (Bin_op (Plus, (Const (Int64 4L)),
               (Un_op (IS_NOT_NULL, (Un_op (IS_NULL, (Var "a")))))))
            )))
         ]
       )) |}]
;;

let%expect_test "Boolean oparators and literals" =
  parse_and_print
    {| not 1 = 1 or 4 = a + 23.0 and false xor not true or "Hello" = a + "llo" |};
  [%expect
    {|
    (Bin_op (OR,
       (Bin_op (OR,
          (Un_op (NOT, (List_op ((Const (Int64 1L)), [(Eq, (Const (Int64 1L)))]))
             )),
          (Bin_op (AND,
             (List_op ((Const (Int64 4L)),
                [(Eq, (Bin_op (Plus, (Var "a"), (Const (Float 23.)))))])),
             (Bin_op (XOR, (Liter False), (Un_op (NOT, (Liter True)))))))
          )),
       (List_op ((Const (String "Hello")),
          [(Eq, (Bin_op (Plus, (Var "a"), (Const (String "llo")))))]))
       )) |}]
;;
