(** Copyright 2021-2023, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DRozhkov_lib
open Ast
open Parser
open Base

let test_parse str =
  match parse str with
  | Result.Ok program -> Format.printf "%a\n" pp_expressions program
  | Result.Error str -> Format.printf "Parsing error%s\n" str
;;

let%expect_test _ =
  test_parse {|
        let x = [1; 2; 3]
      |};
  [%expect
    {|
        [(Decl
            (NoRec, "x",
             (EList ((EConst (Int 1)),
                (EList ((EConst (Int 2)), (EList ((EConst (Int 3)), Nil))))))))
          ]
    |}]
;;

let%expect_test _ =
  test_parse {|
        let x = let a = 10 in a * 10
      |};
  [%expect
    {|
        [(Decl
            (NoRec, "x",
             (ELet (NoRec, "a", (EConst (Int 10)),
                (EBinop ((Var "a"), Mult, (EConst (Int 10))))))))
          ]
    |}]
;;

let%expect_test _ =
  test_parse {|
        5 + 6
      |};
  [%expect {|
        [(Expr (EBinop ((EConst (Int 5)), Plus, (EConst (Int 6)))))]
    |}]
;;
