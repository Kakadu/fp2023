(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let parse_and_print str =
  match Parser.run_parser_program str with
  | Result.Ok program -> Format.printf "%a\n" Ast.pp_program program
  | Result.Error str -> Format.printf "Parsing error%s\n" str
;;

let%expect_test _ =
  parse_and_print {|  ;; ;;;;   ;; ;;let _a=1;; ;;;; let   _b =1+2  |};
  [%expect
    {|
    [(Let_decl (false, (LCIdent "_a"), (Expr_const (Int 1))));
      (Let_decl
         (false, (LCIdent "_b"),
          (Bin_op (Add, (Expr_const (Int 1)), (Expr_const (Int 2))))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let a =  10/( 2+(+ +5) )*( 7 + - 1) - ( 1 + 2*  3 ) |};
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
  parse_and_print {|let rec fac n = if n <= 1 then 1 else n * fac (n - 1)|};
  [%expect
    {|
    [(Let_decl
        (true, (LCIdent "fac"),
         (Expr_fun ((Pat_val (LCIdent "n")),
            (Expr_ite (
               (Bin_op (Leq, (Expr_val (LCIdent "n")), (Expr_const (Int 1)))),
               (Expr_const (Int 1)),
               (Bin_op (Mul, (Expr_val (LCIdent "n")),
                  (Expr_app ((Expr_val (LCIdent "fac")),
                     (Bin_op (Sub, (Expr_val (LCIdent "n")), (Expr_const (Int 1))
                        ))
                     ))
                  ))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let f = fun a b -> fun z -> a + b * z|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_fun ((Pat_val (LCIdent "a")),
            (Expr_fun ((Pat_val (LCIdent "b")),
               (Expr_fun ((Pat_val (LCIdent "z")),
                  (Bin_op (Add, (Expr_val (LCIdent "a")),
                     (Bin_op (Mul, (Expr_val (LCIdent "b")),
                        (Expr_val (LCIdent "z"))))
                     ))
                  ))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print
    {|let f = if true then if true then a else b else if false then x else y|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_ite ((Expr_const (Bool true)),
            (Expr_ite ((Expr_const (Bool true)), (Expr_val (LCIdent "a")),
               (Expr_val (LCIdent "b")))),
            (Expr_ite ((Expr_const (Bool false)), (Expr_val (LCIdent "x")),
               (Expr_val (LCIdent "y"))))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print
    {|let a = let id x = x in let f = fun x -> if x > 0 then t else id b in f 1|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "a"),
         (Expr_let (
            (false, (LCIdent "id"),
             (Expr_fun ((Pat_val (LCIdent "x")), (Expr_val (LCIdent "x"))))),
            (Expr_let (
               (false, (LCIdent "f"),
                (Expr_fun ((Pat_val (LCIdent "x")),
                   (Expr_ite (
                      (Bin_op (Gre, (Expr_val (LCIdent "x")),
                         (Expr_const (Int 0)))),
                      (Expr_val (LCIdent "t")),
                      (Expr_app ((Expr_val (LCIdent "id")),
                         (Expr_val (LCIdent "b"))))
                      ))
                   ))),
               (Expr_app ((Expr_val (LCIdent "f")), (Expr_const (Int 1))))))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let tuple1 = (1,x, 1+2 , (1, 2, 3))|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "tuple1"),
         (Expr_tuple ((Expr_const (Int 1)),
            [(Expr_val (LCIdent "x"));
              (Bin_op (Add, (Expr_const (Int 1)), (Expr_const (Int 2))));
              (Expr_tuple ((Expr_const (Int 1)),
                 [(Expr_const (Int 2)); (Expr_const (Int 3))]))
              ]
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let a =5::4::[1; 2; 3]|};
  [%expect
    {|
      [(Let_decl
          (false, (LCIdent "a"),
           (Expr_cons_list ((Expr_const (Int 5)),
              (Expr_cons_list ((Expr_const (Int 4)),
                 (Expr_cons_list ((Expr_const (Int 1)),
                    (Expr_cons_list ((Expr_const (Int 2)),
                       (Expr_cons_list ((Expr_const (Int 3)), Expr_empty_list))))
                    ))
                 ))
              ))))
        ] |}]
;;

let%expect_test _ =
  parse_and_print
    {|let f x = match x with
      | h::tl -> let x = 1 in x
      | _ -> let id = fun x -> x in
         if (0 < 1) then 2 else id 3|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_fun ((Pat_val (LCIdent "x")),
            (Expr_match ((Expr_val (LCIdent "x")),
               [((Pat_cons_list ((Pat_val (LCIdent "h")),
                    (Pat_val (LCIdent "tl")))),
                 (Expr_let ((false, (LCIdent "x"), (Expr_const (Int 1))),
                    (Expr_val (LCIdent "x")))));
                 (Pat_any,
                  (Expr_let (
                     (false, (LCIdent "id"),
                      (Expr_fun ((Pat_val (LCIdent "x")),
                         (Expr_val (LCIdent "x"))))),
                     (Expr_ite (
                        (Bin_op (Less, (Expr_const (Int 0)), (Expr_const (Int 1))
                           )),
                        (Expr_const (Int 2)),
                        (Expr_app ((Expr_val (LCIdent "id")),
                           (Expr_const (Int 3))))
                        ))
                     )))
                 ]
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let k = let f ((t::1::(1,2)::y), 3) = a in f|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "k"),
         (Expr_let (
            (false, (LCIdent "f"),
             (Expr_fun (
                (Pat_tuple (
                   (Pat_cons_list ((Pat_val (LCIdent "t")),
                      (Pat_cons_list ((Pat_const (Int 1)),
                         (Pat_cons_list (
                            (Pat_tuple ((Pat_const (Int 1)),
                               [(Pat_const (Int 2))])),
                            (Pat_val (LCIdent "y"))))
                         ))
                      )),
                   [(Pat_const (Int 3))])),
                (Expr_val (LCIdent "a"))))),
            (Expr_val (LCIdent "f"))))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let f = printf "sdfsdf"; prtinf "sfsf%c %d";; [323;32]|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_seq (
            (Expr_app ((Expr_val (LCIdent "printf")),
               (Expr_const (String "sdfsdf")))),
            (Expr_app ((Expr_val (LCIdent "prtinf")),
               (Expr_const (String "sfsf%c %d"))))
            ))));
      (Expr
         (Expr_cons_list ((Expr_const (Int 323)),
            (Expr_cons_list ((Expr_const (Int 32)), Expr_empty_list)))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|1; 2 let x = 1;; let a = 2; 3|};
  [%expect
    {|
    [(Expr (Expr_seq ((Expr_const (Int 1)), (Expr_const (Int 2)))));
      (Let_decl (false, (LCIdent "x"), (Expr_const (Int 1))));
      (Let_decl
         (false, (LCIdent "a"),
          (Expr_seq ((Expr_const (Int 2)), (Expr_const (Int 3))))))
      ] |}]
;;

let%expect_test _ =
  let _ = parse_and_print {| let str = "sdfs"; printf "%c" str.[0]; [] |} in
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "str"),
         (Expr_seq ((Expr_const (String "sdfs")),
            (Expr_seq (
               (Expr_app (
                  (Expr_app ((Expr_val (LCIdent "printf")),
                     (Expr_const (String "%c")))),
                  (Expr_app (
                     (Expr_app ((Expr_val (LCIdent "get")),
                        (Expr_val (LCIdent "str")))),
                     (Expr_const (Int 0))))
                  )),
               Expr_empty_list))
            ))))
      ] |}]
;;

let%expect_test _ =
  let _ = parse_and_print {| printf "%s" str |} in
  [%expect
    {|
    [(Expr
        (Expr_app (
           (Expr_app ((Expr_val (LCIdent "printf")), (Expr_const (String "%s")))),
           (Expr_val (LCIdent "str")))))
      ] |}]
;;

let%expect_test _ =
  let _ = parse_and_print {|let str = "\n\\\"\t\n"|} in
  [%expect
    {|
    [(Let_decl (false, (LCIdent "str"), (Expr_const (String "\n\\\"\t\n"))))] |}]
;;

(* errors*)

let%expect_test _ =
  let _ = parse_and_print {| let a = 2 +- 3;; let b =+2  |} in
  [%expect {|
    Parsing error: end_of_input |}]
;;

let%expect_test _ =
  let _ = parse_and_print {|let str = "\x"|} in
  [%expect {|
    Parsing error: no more choices |}]
;;

let%expect_test _ =
  let _ = parse_and_print {|let f (a, b) = a + b;; let a = f (1, 2)|} in
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_fun (
            (Pat_tuple ((Pat_val (LCIdent "a")), [(Pat_val (LCIdent "b"))])),
            (Bin_op (Add, (Expr_val (LCIdent "a")), (Expr_val (LCIdent "b"))))))));
      (Let_decl
         (false, (LCIdent "a"),
          (Expr_app ((Expr_val (LCIdent "f")),
             (Expr_tuple ((Expr_const (Int 1)), [(Expr_const (Int 2))]))))))
      ] |}]
;;
