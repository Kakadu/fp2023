(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Bynary operatios *)

let%expect_test _ =
  parse_with_print {| 1 + 5 * 3 |};
  [%expect
    {|
    [(SExpression
        (EBinaryOperation (Add, (EConst (Int 1)),
           (EBinaryOperation (Mul, (EConst (Int 5)), (EConst (Int 3)))))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| 1 * (+5) / (-3) |};
  [%expect
    {|
    [(SExpression
        (EBinaryOperation (Div,
           (EBinaryOperation (Mul, (EConst (Int 1)),
              (EUnaryOperation (Plus, (EConst (Int 5)))))),
           (EUnaryOperation (Minus, (EConst (Int 3)))))))
      ] |}]
;;

(* ---------------- *)

(* Unary operations *)

let%expect_test _ =
  parse_with_print {| -1 |};
  [%expect {|
    [(SExpression (EUnaryOperation (Minus, (EConst (Int 1)))))] |}]
;;

let%expect_test _ =
  parse_with_print {| not true |};
  [%expect {|
    [(SExpression (EUnaryOperation (Not, (EConst (Bool true)))))] |}]
;;

let%expect_test _ =
  parse_with_print {| +(-(+3)) |};
  [%expect
    {|
    [(SExpression
        (EUnaryOperation (Plus,
           (EUnaryOperation (Minus, (EUnaryOperation (Plus, (EConst (Int 3))))))
           )))
      ] |}]
;;

(* ---------------- *)

(* If Then Else *)

let%expect_test _ =
  parse_with_print {| if false || true && not false then "Yes" else "No" |};
  [%expect
    {|
    [(SExpression
        (EIfThenElse (
           (EBinaryOperation (Or, (EConst (Bool false)),
              (EBinaryOperation (And, (EConst (Bool true)),
                 (EUnaryOperation (Not, (EConst (Bool false))))))
              )),
           (EConst (String "Yes")), (EConst (String "No")))))
      ] |}]
;;

(* ---------------- *)
