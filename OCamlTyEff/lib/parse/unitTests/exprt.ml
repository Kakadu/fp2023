(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Pp
open Expr

let%expect_test "parse_function_pattern_matching" =
  pp pp_expression parse_expression "function | a -> true | b -> false" ;
  [%expect
    {|
    (Exp_function
       [{ left = (Pat_var "a"); right = (Exp_construct ((Ident "true"), None)) };
         { left = (Pat_var "b"); right = (Exp_construct ((Ident "false"), None))
           }
         ]) |}]

let%expect_test "parse_lambda_fun" =
  pp pp_expression parse_expression "fun x y -> x + y" ;
  [%expect
    {|
    (Exp_fun ([(Pat_var "x"); (Pat_var "y")],
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
          (Exp_ident (Ident "y"))))
       )) |}]

let%expect_test "parse_custom_operator" =
  pp pp_expression parse_expression "a >>= b ++ c ** d !+ e" ;
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident (Ident ">>=")), (Exp_ident (Ident "a")))),
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "++")), (Exp_ident (Ident "b")))),
          (Exp_apply (
             (Exp_apply ((Exp_ident (Ident "**")), (Exp_ident (Ident "c")))),
             (Exp_apply ((Exp_ident (Ident "d")),
                (Exp_apply ((Exp_ident (Ident "!+")), (Exp_ident (Ident "e"))))))
             ))
          ))
       )) |}]

let%expect_test "parse_let" =
  pp pp_expression parse_expression "let rec a = 1 and b = 2 in let e = 3 in a" ;
  [%expect
    {|
    (Exp_let (Recursive,
       [{ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) };
         { pat = (Pat_var "b"); expr = (Exp_constant (Const_integer 2)) }],
       (Exp_let (Nonrecursive,
          [{ pat = (Pat_var "e"); expr = (Exp_constant (Const_integer 3)) }],
          (Exp_ident (Ident "a"))))
       )) |}]

let%expect_test "parse_ifthenelse" =
  pp pp_expression parse_expression "if a then (if b then c) else d" ;
  [%expect
    {|
    (Exp_ifthenelse ((Exp_ident (Ident "a")),
       (Exp_ifthenelse ((Exp_ident (Ident "b")), (Exp_ident (Ident "c")), None)),
       (Some (Exp_ident (Ident "d"))))) |}]

let%expect_test "parse_if_with_seq1" =
  pp pp_expression parse_expression "if a; b then c; d" ;
  [%expect
    {|
    (Exp_sequence (
       (Exp_ifthenelse (
          (Exp_sequence ((Exp_ident (Ident "a")), (Exp_ident (Ident "b")))),
          (Exp_ident (Ident "c")), None)),
       (Exp_ident (Ident "d")))) |}]

let%expect_test "parse_if_with_seq2" =
  pp pp_expression parse_expression "if a; b then (c; d)" ;
  [%expect
    {|
    (Exp_ifthenelse (
       (Exp_sequence ((Exp_ident (Ident "a")), (Exp_ident (Ident "b")))),
       (Exp_sequence ((Exp_ident (Ident "c")), (Exp_ident (Ident "d")))), None)) |}]

let%expect_test "parse_match1" =
  pp pp_expression parse_expression "match a with b -> c | d -> e" ;
  [%expect
    {|
    (Exp_match ((Exp_ident (Ident "a")),
       [{ left = (Pat_var "b"); right = (Exp_ident (Ident "c")) };
         { left = (Pat_var "d"); right = (Exp_ident (Ident "e")) }]
       )) |}]

let%expect_test "parse_match2" =
  pp pp_expression parse_expression "match a with | b | c | d -> e | f -> g" ;
  [%expect
    {|
    (Exp_match ((Exp_ident (Ident "a")),
       [{ left =
          (Pat_or ((Pat_or ((Pat_var "b"), (Pat_var "c"))), (Pat_var "d")));
          right = (Exp_ident (Ident "e")) };
         { left = (Pat_var "f"); right = (Exp_ident (Ident "g")) }]
       )) |}]

let%expect_test "parse_constr1" =
  pp pp_expression parse_expression "Nil" ;
  [%expect {| (Exp_construct ((Ident "Nil"), None)) |}]

let%expect_test "parse_constr2" =
  pp pp_expression parse_expression "Some x" ;
  [%expect
    {| (Exp_construct ((Ident "Some"), (Some (Exp_ident (Ident "x"))))) |}]

let%expect_test "parse_constr3" =
  pp pp_expression parse_expression "Cons (1, Nil)" ;
  [%expect
    {|
    (Exp_construct ((Ident "Cons"),
       (Some (Exp_tuple
                [(Exp_constant (Const_integer 1));
                  (Exp_construct ((Ident "Nil"), None))]))
       )) |}]

let%expect_test "parse_list" =
  pp pp_expression parse_expression "[a;b;c]" ;
  [%expect
    {|
    (Exp_construct ((Ident "::"),
       (Some (Exp_tuple
                [(Exp_ident (Ident "a"));
                  (Exp_construct ((Ident "::"),
                     (Some (Exp_tuple
                              [(Exp_ident (Ident "b"));
                                (Exp_construct ((Ident "::"),
                                   (Some (Exp_tuple
                                            [(Exp_ident (Ident "c"));
                                              (Exp_construct ((Ident "[]"), None
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_list_with_seq" =
  pp pp_expression parse_expression "[a;(b;c)]" ;
  [%expect
    {|
    (Exp_construct ((Ident "::"),
       (Some (Exp_tuple
                [(Exp_ident (Ident "a"));
                  (Exp_construct ((Ident "::"),
                     (Some (Exp_tuple
                              [(Exp_sequence ((Exp_ident (Ident "b")),
                                  (Exp_ident (Ident "c"))));
                                (Exp_construct ((Ident "[]"), None))]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_list_1element" =
  pp pp_expression parse_expression "[a]" ;
  [%expect
    {|
    (Exp_construct ((Ident "::"),
       (Some (Exp_tuple
                [(Exp_ident (Ident "a")); (Exp_construct ((Ident "[]"), None))]))
       )) |}]

let%expect_test "parse_list_empty" =
  pp pp_expression parse_expression "[]" ;
  [%expect {| (Exp_construct ((Ident "[]"), None)) |}]

let%expect_test "parse_list_op" =
  pp pp_expression parse_expression "(a :: b) :: c :: d :: []" ;
  [%expect
    {|
    (Exp_construct ((Ident "::"),
       (Some (Exp_tuple
                [(Exp_construct ((Ident "::"),
                    (Some (Exp_tuple
                             [(Exp_ident (Ident "a")); (Exp_ident (Ident "b"))]))
                    ));
                  (Exp_construct ((Ident "::"),
                     (Some (Exp_tuple
                              [(Exp_ident (Ident "c"));
                                (Exp_construct ((Ident "::"),
                                   (Some (Exp_tuple
                                            [(Exp_ident (Ident "d"));
                                              (Exp_construct ((Ident "[]"), None
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_seq_op" =
  pp pp_expression parse_expression "(a ; b) ; c ; d ; e" ;
  [%expect
    {|
    (Exp_sequence (
       (Exp_sequence ((Exp_ident (Ident "a")), (Exp_ident (Ident "b")))),
       (Exp_sequence ((Exp_ident (Ident "c")),
          (Exp_sequence ((Exp_ident (Ident "d")), (Exp_ident (Ident "e"))))))
       )) |}]

let%expect_test "parse_tuple_op" =
  pp pp_expression parse_expression "a, (b, c), d, e" ;
  [%expect
    {|
    (Exp_tuple
       [(Exp_ident (Ident "a"));
         (Exp_tuple [(Exp_ident (Ident "b")); (Exp_ident (Ident "c"))]);
         (Exp_ident (Ident "d")); (Exp_ident (Ident "e"))]) |}]

let%expect_test "parse_plus_minus_prefix_op" =
  pp pp_expression parse_expression "1 + - + + 3" ;
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident (Ident "+")), (Exp_constant (Const_integer 1)))),
       (Exp_apply ((Exp_ident (Ident "~-")),
          (Exp_apply ((Exp_ident (Ident "~+")),
             (Exp_apply ((Exp_ident (Ident "~+")),
                (Exp_constant (Const_integer 3))))
             ))
          ))
       )) |}]

let%expect_test "parse_custom_prefix_op" =
  pp pp_expression parse_expression "?!5; !%< 123; !0; ~-3" ;
  [%expect
    {|
    (Exp_sequence (
       (Exp_apply ((Exp_ident (Ident "?!")), (Exp_constant (Const_integer 5)))),
       (Exp_sequence (
          (Exp_apply ((Exp_ident (Ident "!%<")),
             (Exp_constant (Const_integer 123)))),
          (Exp_sequence (
             (Exp_apply ((Exp_ident (Ident "!")),
                (Exp_constant (Const_integer 0)))),
             (Exp_apply ((Exp_ident (Ident "~-")),
                (Exp_constant (Const_integer 3))))
             ))
          ))
       )) |}]
