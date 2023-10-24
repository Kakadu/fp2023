(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib
open Ast
open Parser

let ptest parser printer s =
  match Angstrom.parse_string ~consume:All parser s with
  | Ok a -> Format.printf "%a" printer a
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  ptest pexpr pp_expr "1 + 2 - 3 * 4 + (-5) / 6 * 7 - 8 * 9";
  [%expect
    {|
      (ExprBinOp (Sub,
         (ExprBinOp (Add,
            (ExprBinOp (Sub,
               (ExprBinOp (Add, (ExprLit (LitInt 1)), (ExprLit (LitInt 2)))),
               (ExprBinOp (Mul, (ExprLit (LitInt 3)), (ExprLit (LitInt 4)))))),
            (ExprBinOp (Mul,
               (ExprBinOp (Div, (ExprUnOp (Neg, (ExprLit (LitInt 5)))),
                  (ExprLit (LitInt 6)))),
               (ExprLit (LitInt 7))))
            )),
         (ExprBinOp (Mul, (ExprLit (LitInt 8)), (ExprLit (LitInt 9)))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl "x = (1, 2)";
  [%expect
    {|
    (DeclLet
       ((PatVar "x"), (ExprTuple [(ExprLit (LitInt 1)); (ExprLit (LitInt 2))]))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl "fact n = if (n < 2) then 1 else fact (n - 1) * n";
  [%expect
    {| 
    (DeclLet
       ((PatVar "fact"),
        (ExprFunc
           ((PatVar "n"),
            (ExprIf ((ExprBinOp (Lt, (ExprVar "n"), (ExprLit (LitInt 2)))),
               (ExprLit (LitInt 1)),
               (ExprBinOp (Mul,
                  (ExprApp ((ExprVar "fact"),
                     (ExprBinOp (Sub, (ExprVar "n"), (ExprLit (LitInt 1)))))),
                  (ExprVar "n")))
               )))))) |}]
;;
