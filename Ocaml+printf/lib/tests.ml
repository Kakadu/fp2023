(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let parse_and_print str =
  Format.printf "%a" Ast.pp_program (Result.get_ok (Parser.parse_program str))
;;

let%expect_test _ =
  parse_and_print "  ;; ;;;;   ;; ;;let _a=1;; ;;;; let   _b =1+2  ";
  [%expect
    {|
    [(Let_decl (false, (LCIdent "_a"), (Expr_const (Int 1))));
      (Let_decl
         (false, (LCIdent "_b"),
          (Bin_op (Add, (Expr_const (Int 1)), (Expr_const (Int 2))))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print "let abcde a  b  c  d  e   =1     ";
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "abcde"),
         (Fun ((LCIdent "a"),
            (Fun ((LCIdent "b"),
               (Fun ((LCIdent "c"),
                  (Fun ((LCIdent "d"),
                     (Fun ((LCIdent "e"), (Expr_const (Int 1))))))
                  ))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print "let a =  10/( 2+(+ +5) )*( 7 + - 1) - ( 1 + 2*  3 ) ";
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "a"),
         (Bin_op (Sub,
            (Bin_op (Mul,
               (Bin_op (Div, (Expr_const (Int 10)),
                  (Bin_op (Add, (Expr_const (Int 2)),
                     (Un_op (Un_plus, (Un_op (Un_plus, (Expr_const (Int 5))))))))
                  )),
               (Bin_op (Add, (Expr_const (Int 7)),
                  (Un_op (Un_minus, (Expr_const (Int 1))))))
               )),
            (Bin_op (Add, (Expr_const (Int 1)),
               (Bin_op (Mul, (Expr_const (Int 2)), (Expr_const (Int 3))))))
            ))))
      ] |}]
;;

(* factorial *)
let%expect_test _ =
  parse_and_print "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)";
  [%expect
    {|
    [(Let_decl
        (true, (LCIdent "fac"),
         (Fun ((LCIdent "n"),
            (ITE ((Bin_op (Leq, (Expr_val (LCIdent "n")), (Expr_const (Int 1)))),
               (Expr_const (Int 1)),
               (Bin_op (Mul, (Expr_val (LCIdent "n")),
                  (App ((Expr_val (LCIdent "fac")),
                     (Bin_op (Sub, (Expr_val (LCIdent "n")), (Expr_const (Int 1))
                        ))
                     ))
                  ))
               ))
            ))))
      ] |}]
;;
