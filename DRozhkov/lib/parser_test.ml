(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Base

let test_parse input =
  let open Stdlib.Format in
  match parser input with
  | Ok ast -> printf "%a\n" pp_expr ast
  | Error s -> printf "Parsing error: %s\n" s
;;

let%expect_test _ =
  test_parse
    {|
        let x = 5 + 6
      |};

  [%expect
    {|
      (ELet (NoRec, "x", (EBinop ((EConst (Int 5)), Plus, (EConst (Int 6)))),
         Nothing))
    |}]
;;

let%expect_test _ =
  test_parse
    {|
        let x = 5 in
        let rec fact = if x < 1 then 1 else fact (x - 1)
      |};

  [%expect
    {|
        (ELet (NoRec, "x", (EConst (Int 5)),
           (ELet (Rec, "fact",
              (EIfThenElse ((EBinop ((Var "x"), Less, (EConst (Int 1)))),
                 (EConst (Int 1)),
                 (EApp ((Var "fact"), (EBinop ((Var "x"), Minus, (EConst (Int 1))))))
                 )),
              Nothing))
           ))
    |}]
;;

let%expect_test _ =
  test_parse
    {|
        let x = [1; 2; 3]
      |};

  [%expect
    {|
        (ELet (NoRec, "x",
           (EList [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3))]), Nothing))
    |}]
;;


