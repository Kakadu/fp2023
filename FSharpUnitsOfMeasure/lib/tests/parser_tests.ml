(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast
open Parser

let parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "%s" e
;;

(* Types test *)

let%expect_test _ =
  parsed_result "777" parse_types show_types;
  [%expect {| (FInt 777) |}]
;;

let%expect_test _ =
  parsed_result "-777" parse_types show_types;
  [%expect {| (FInt -777) |}]
;;

let%expect_test _ =
  parsed_result "777.777" parse_types show_types;
  [%expect {| (FFloat 777.777) |}]
;;

let%expect_test _ =
  parsed_result "-777.777" parse_types show_types;
  [%expect {| (FFloat -777.777) |}]
;;

let%expect_test _ =
  parsed_result "true" parse_types show_types;
  [%expect {| (FBool true) |}]
;;

let%expect_test _ =
  parsed_result "false" parse_types show_types;
  [%expect {| (FBool false) |}]
;;

let%expect_test _ =
  parsed_result "\"Test to string\"" parse_types show_types;
  [%expect {| (FString "Test to string") |}]
;;

let%expect_test _ =
  parsed_result "()" parse_types show_types;
  [%expect {| FUnit |}]
;;

let%expect_test _ =
  parsed_result "[]" parse_types show_types;
  [%expect {| FNil |}]
;;

let%expect_test _ =
  parsed_result "7.77<sec>" parse_types show_types;
  [%expect {|
    (Measure_float ((FFloat 7.77), (SMeasure ("sec", (Pow (FInt 1)))))) |}]
;;

let%expect_test _ =
  parsed_result "7.77<m/sec>" parse_types show_types;
  [%expect
    {|
    (Measure_float ((FFloat 7.77),
       (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
          (SMeasure ("sec", (Pow (FInt 1))))))
       )) |}]
;;

let%expect_test _ =
  parsed_result "7.77" parse_types show_types;
  [%expect {| (FFloat 7.77) |}]
;;

(** Pattern test *)

let%expect_test _ =
  parsed_result "1" parse_pattern show_pattern;
  [%expect {| (PConst (FInt 1)) |}]
;;

let%expect_test _ =
  parsed_result "a" parse_pattern show_pattern;
  [%expect {| (PVar "a") |}]
;;

let%expect_test _ =
  parsed_result "a, b" parse_pattern show_pattern;
  [%expect {| (PTuple [(PVar "a"); (PVar "b")]) |}]
;;

let%expect_test _ =
  parsed_result "((a, b), c)" parse_pattern show_pattern;
  [%expect {| (PTuple [(PTuple [(PVar "a"); (PVar "b")]); (PVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "((a, 1), c)" parse_pattern show_pattern;
  [%expect {| (PTuple [(PTuple [(PVar "a"); (PConst (FInt 1))]); (PVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "a :: b" parse_pattern show_pattern;
  [%expect {| (PCons ((PVar "a"), (PVar "b"))) |}]
;;

let%expect_test _ =
  parsed_result "() :: []" parse_pattern show_pattern;
  [%expect {| (PCons ((PConst FUnit), (PConst FNil))) |}]
;;

let%expect_test _ =
  parsed_result "[5]" parse_pattern show_pattern;
  [%expect {| (PList [(PConst (FInt 5))]) |}]
;;

let%expect_test _ =
  parsed_result "[1; 2]" parse_pattern show_pattern;
  [%expect {| (PList [(PConst (FInt 1)); (PConst (FInt 2))]) |}]
;;

let%expect_test _ =
  parsed_result "[ 1; 2]" parse_pattern show_pattern;
  [%expect {| (PList [(PConst (FInt 1)); (PConst (FInt 2))]) |}]
;;

let%expect_test _ =
  parsed_result "[ 1; 2 ]" parse_pattern show_pattern;
  [%expect {| (PList [(PConst (FInt 1)); (PConst (FInt 2))]) |}]
;;

let%expect_test _ =
  parsed_result "[1; 2; a]" parse_pattern show_pattern;
  [%expect {| (PList [(PConst (FInt 1)); (PConst (FInt 2)); (PVar "a")]) |}]
;;

(** Expressions with constant *)

let%expect_test _ =
  parsed_result "777" parse_expression show_expression;
  [%expect {| (EConst (FInt 777)) |}]
;;

let%expect_test _ =
  parsed_result "-777" parse_expression show_expression;
  [%expect {| (EConst (FInt -777)) |}]
;;

let%expect_test _ =
  parsed_result "777.777" parse_expression show_expression;
  [%expect {| (EConst (FFloat 777.777)) |}]
;;

let%expect_test _ =
  parsed_result "-777.777" parse_expression show_expression;
  [%expect {| (EConst (FFloat -777.777)) |}]
;;

let%expect_test _ =
  parsed_result "true" parse_expression show_expression;
  [%expect {| (EConst (FBool true)) |}]
;;

let%expect_test _ =
  parsed_result "false" parse_expression show_expression;
  [%expect {| (EConst (FBool false)) |}]
;;

let%expect_test _ =
  parsed_result "\"Test to string\"" parse_expression show_expression;
  [%expect {| (EConst (FString "Test to string")) |}]
;;

let%expect_test _ =
  parsed_result "()" parse_expression show_expression;
  [%expect {| (EConst FUnit) |}]
;;

let%expect_test _ =
  parsed_result "[]" parse_expression show_expression;
  [%expect {| (EConst FNil) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type cm" parse_expression show_expression;
  [%expect {| (EMeasure (SMeasure_init (SMeasure ("cm", (Pow (FInt 1)))))) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type m ^ -2" parse_expression show_expression;
  [%expect {| (EMeasure (SMeasure_init (SMeasure ("m", (Pow (FInt -2)))))) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type spped = m^2/sec" parse_expression show_expression;
  [%expect
    {|
    (EMeasure
       (MMeasure_init ((SMeasure ("spped", (Pow (FInt 1)))),
          (MMeasure ((SMeasure ("m", (Pow (FInt 2)))), Div,
             (SMeasure ("sec", (Pow (FInt 1))))))
          ))) |}]
;;

let%expect_test _ =
  parsed_result "7.77<sec>" parse_expression show_expression;
  [%expect
    {|
    (EConst (Measure_float ((FFloat 7.77), (SMeasure ("sec", (Pow (FInt 1))))))) |}]
;;

let%expect_test _ =
  parsed_result "7.77<m/sec>" parse_expression show_expression;
  [%expect
    {|
    (EConst
       (Measure_float ((FFloat 7.77),
          (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
             (SMeasure ("sec", (Pow (FInt 1))))))
          ))) |}]
;;

let%expect_test _ =
  parsed_result "7.77" parse_expression show_expression;
  [%expect {| (EConst (FFloat 7.77)) |}]
;;

(** Expressions with binary operations test *)

let%expect_test _ =
  parsed_result "1 - 2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Sub), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "z * v" parse_expression show_expression;
  [%expect {|
    (EApp ((EBinaryOp Mul), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z / v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Div), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z % v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mod), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z && v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp And), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z || v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Or), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z = v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Eq), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z < v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Less), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z > v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Gre), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z <= v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Leq), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "z >= v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Greq), (EApp ((EVar "z"), (EVar "v"))))) |}]
;;

let%expect_test _ =
  parsed_result "1 * 2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1 <> 2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Neq), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1 * -2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt -2)))))) |}]
;;

let%expect_test _ =
  parsed_result "z * (v / y)" parse_expression show_expression;
  [%expect
    {| 
  (EApp ((EBinaryOp Mul),
     (EApp ((EVar "z"),
        (EApp ((EBinaryOp Div), (EApp ((EVar "v"), (EVar "y")))))))
     ))
     |}]
;;

let%expect_test _ =
  parsed_result "(z && v) || x" parse_expression show_expression;
  [%expect
    {|
     (EApp ((EBinaryOp Or),
        (EApp ((EApp ((EBinaryOp And), (EApp ((EVar "z"), (EVar "v"))))),
           (EVar "x")))
        ))
        |}]
;;

let%expect_test _ =
  parsed_result "1 + 3 - 5" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Sub),
       (EApp (
          (EApp ((EBinaryOp Add), (EApp ((EConst (FInt 1)), (EConst (FInt 3)))))),
          (EConst (FInt 5))))
       )) |}]
;;

let%expect_test _ =
  parsed_result "(3 + 1) * (4 - 2) * (9 / 3)" parse_expression show_expression;
  [%expect
    {|
     (EApp ((EBinaryOp Mul),
        (EApp (
           (EApp ((EBinaryOp Mul),
              (EApp (
                 (EApp ((EBinaryOp Add),
                    (EApp ((EConst (FInt 3)), (EConst (FInt 1)))))),
                 (EApp ((EBinaryOp Sub),
                    (EApp ((EConst (FInt 4)), (EConst (FInt 2))))))
                 ))
              )),
           (EApp ((EBinaryOp Div), (EApp ((EConst (FInt 9)), (EConst (FInt 3))))))
           ))
        ))
        |}]
;;

(** Expressions with tuple test *)

let%expect_test _ =
  parsed_result "(1.1, 5)" parse_expression show_expression;
  [%expect {| (ETuple [(EConst (FFloat 1.1)); (EConst (FInt 5))]) |}]
;;

let%expect_test _ =
  parsed_result "let x = (1, 2)" parse_expression show_expression;
  [%expect {| (ELet ("NotRec", "x", (ETuple [(EConst (FInt 1)); (EConst (FInt 2))]))) |}]
;;

let%expect_test _ =
  parsed_result "((a, 69.96), c)" parse_expression show_expression;
  [%expect {| (ETuple [(ETuple [(EVar "a"); (EConst (FFloat 69.96))]); (EVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "(1,2,3) = (1,2,3)" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Eq),
       (EApp ((ETuple [(EConst (FInt 1)); (EConst (FInt 2)); (EConst (FInt 3))]),
          (ETuple [(EConst (FInt 1)); (EConst (FInt 2)); (EConst (FInt 3))])))
       )) |}]
;;

let%expect_test _ =
  parsed_result "a = (1,2,3)" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Eq),
       (EApp ((EVar "a"),
          (ETuple [(EConst (FInt 1)); (EConst (FInt 2)); (EConst (FInt 3))])))
       )) |}]
;;

let%expect_test _ =
  parsed_result "((a, 1), c)" parse_expression show_expression;
  [%expect {| (ETuple [(ETuple [(EVar "a"); (EConst (FInt 1))]); (EVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "((a, 1), ((1.2, b), (c, 3.44)))" parse_expression show_expression;
  [%expect
    {|
    (ETuple
       [(ETuple [(EVar "a"); (EConst (FInt 1))]);
         (ETuple
            [(ETuple [(EConst (FFloat 1.2)); (EVar "b")]);
              (ETuple [(EVar "c"); (EConst (FFloat 3.44))])])
         ]) |}]
;;

(** Expressions with list test *)

let%expect_test _ =
  parsed_result "[1; 5]" parse_expression show_expression;
  [%expect {| (EList [(EConst (FInt 1)); (EConst (FInt 5))]) |}]
;;

let%expect_test _ =
  parsed_result "[1.1; 5]" parse_expression show_expression;
  [%expect {| (EList [(EConst (FFloat 1.1)); (EConst (FInt 5))]) |}]
;;

let%expect_test _ =
  parsed_result "[1.1; 5] = [1.1; 5]" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Eq),
       (EApp ((EList [(EConst (FFloat 1.1)); (EConst (FInt 5))]),
          (EList [(EConst (FFloat 1.1)); (EConst (FInt 5))])))
       )) |}]
;;

let%expect_test _ =
  parsed_result "[[69.96 ; b]; c]" parse_expression show_expression;
  [%expect {| (EList [(EList [(EConst (FFloat 69.96)); (EVar "b")]); (EVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "[[a; 1]; c]" parse_expression show_expression;
  [%expect {| (EList [(EList [(EVar "a"); (EConst (FInt 1))]); (EVar "c")]) |}]
;;

let%expect_test _ =
  parsed_result "[1.1; 5] = z" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Eq),
       (EApp ((EList [(EConst (FFloat 1.1)); (EConst (FInt 5))]), (EVar "z"))))) |}]
;;

let%expect_test _ =
  parsed_result "z = [1.1; 5]" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Eq),
       (EApp ((EVar "z"), (EList [(EConst (FFloat 1.1)); (EConst (FInt 5))]))))) |}]
;;

let%expect_test _ =
  parsed_result "[[a; 1]; [[1.2; b]; [c; 3]]]" parse_expression show_expression;
  [%expect
    {|
    (EList
       [(EList [(EVar "a"); (EConst (FInt 1))]);
         (EList
            [(EList [(EConst (FFloat 1.2)); (EVar "b")]);
              (EList [(EVar "c"); (EConst (FInt 3))])])
         ]) |}]
;;

(** Expressions with list + tuple test *)

let%expect_test _ =
  parsed_result "([69.69; b], [1; d], [], ())" parse_expression show_expression;
  [%expect
    {|
    (ETuple
       [(EList [(EConst (FFloat 69.69)); (EVar "b")]);
         (EList [(EConst (FInt 1)); (EVar "d")]); (EConst FNil); (EConst FUnit)]) |}]
;;

(** Expression with if ... then ... else ...*)

let%expect_test _ =
  parsed_result "if true then 2 else 1" parse_expression show_expression;
  [%expect {| (EIfElse ((EConst (FBool true)), (EConst (FInt 2)), (EConst (FInt 1)))) |}]
;;

(** Expressions with match test *)

let%expect_test _ =
  parsed_result
    "match x with \n \n   | a -> b \n   | _ -> c"
    parse_expression
    show_expression;
  [%expect
    {|
    (EMatch ((EVar "x"), [((PVar "a"), (EVar "b")); (PWild, (EVar "c"))])) |}]
;;

let%expect_test _ =
  parsed_result
    "let num x = \n    match x with \n     | 1 -> 1 \n     | _ -> 2"
    parse_expression
    show_expression;
  [%expect
    {|
    (ELet ("NotRec", "num",
       (EFun ((PVar "x"),
          (EMatch ((EVar "x"),
             [((PConst (FInt 1)), (EConst (FInt 1))); (PWild, (EConst (FInt 2)))]
             ))
          ))
       )) |}]
;;

(** Expressions with fun test *)

let%expect_test _ =
  parsed_result "fun z -> v" parse_expression show_expression;
  [%expect {|
    (EFun ((PVar "z"), (EVar "v"))) |}]
;;

let%expect_test _ =
  parsed_result "fun z -> 6.66" parse_expression show_expression;
  [%expect {|
    (EFun ((PVar "z"), (EConst (FFloat 6.66)))) |}]
;;

let%expect_test _ =
  parsed_result "(fun x -> x) 5" parse_expression show_expression;
  [%expect {| (EApp ((EFun ((PVar "x"), (EVar "x"))), (EConst (FInt 5)))) |}]
;;

let%expect_test _ =
  parsed_result
    "(((((fun a b c d e -> a + b + c + d + e) 1) 2) 3) 4) "
    parse_expression
    show_expression;
  [%expect
    {|
    (EApp (
       (EApp (
          (EApp (
             (EApp (
                (EFun ((PVar "a"),
                   (EFun ((PVar "b"),
                      (EFun ((PVar "c"),
                         (EFun ((PVar "d"),
                            (EFun ((PVar "e"),
                               (EApp ((EBinaryOp Add),
                                  (EApp (
                                     (EApp ((EBinaryOp Add),
                                        (EApp (
                                           (EApp ((EBinaryOp Add),
                                              (EApp (
                                                 (EApp ((EBinaryOp Add),
                                                    (EApp ((EVar "a"), (EVar "b")
                                                       ))
                                                    )),
                                                 (EVar "c")))
                                              )),
                                           (EVar "d")))
                                        )),
                                     (EVar "e")))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   )),
                (EConst (FInt 1)))),
             (EConst (FInt 2)))),
          (EConst (FInt 3)))),
       (EConst (FInt 4)))) |}]
;;

(** Expressions with let test *)

let%expect_test _ =
  parsed_result "let x = 5" parse_expression show_expression;
  [%expect {| (ELet ("NotRec", "x", (EConst (FInt 5)))) |}]
;;

let%expect_test _ =
  parsed_result "let number x = x " parse_expression show_expression;
  [%expect {| (ELet ("NotRec", "number", (EFun ((PVar "x"), (EVar "x"))))) |}]
;;

let%expect_test _ =
  parsed_result "number 5 " parse_expression show_expression;
  [%expect {| (EApp ((EVar "number"), (EConst (FInt 5)))) |}]
;;

let%expect_test _ =
  parsed_result
    "let increase_by_five z = (fun v -> z + v) 5"
    parse_expression
    show_expression;
  [%expect
    {|
    (ELet ("NotRec", "increase_by_five",
       (EFun ((PVar "z"),
          (EApp (
             (EFun ((PVar "v"),
                (EApp ((EBinaryOp Add), (EApp ((EVar "z"), (EVar "v"))))))),
             (EConst (FInt 5))))
          ))
       )) |}]
;;

let%expect_test _ =
  parsed_result "increase_by_five 10" parse_expression show_expression;
  [%expect {|
    (EApp ((EVar "increase_by_five"), (EConst (FInt 10)))) |}]
;;

(** Expressions with measure test *)

let%expect_test _ =
  parsed_result "[<Measure>] \n type m" parse_expression show_expression;
  [%expect {| (EMeasure (SMeasure_init (SMeasure ("m", (Pow (FInt 1)))))) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type sec" parse_expression show_expression;
  [%expect {| (EMeasure (SMeasure_init (SMeasure ("sec", (Pow (FInt 1)))))) |}]
;;

let%expect_test _ =
  parsed_result "7.73<m> + 7.77<m>" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Add),
       (EApp (
          (EConst
             (Measure_float ((FFloat 7.73), (SMeasure ("m", (Pow (FInt 1))))))),
          (EConst
             (Measure_float ((FFloat 7.77), (SMeasure ("m", (Pow (FInt 1)))))))
          ))
       )) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type sp = speed^-1 * dm" parse_expression show_expression;
  [%expect
    {|
    (EMeasure
       (MMeasure_init ((SMeasure ("sp", (Pow (FInt 1)))),
          (MMeasure ((SMeasure ("speed", (Pow (FInt -1)))), Mul,
             (SMeasure ("dm", (Pow (FInt 1))))))
          ))) |}]
;;

let%expect_test _ =
  parsed_result "[<Measure>] type sp = speed dm" parse_expression show_expression;
  [%expect
    {|
    (EMeasure
       (MMeasure_init ((SMeasure ("sp", (Pow (FInt 1)))),
          (MMeasure ((SMeasure ("speed", (Pow (FInt 1)))), Mul,
             (SMeasure ("dm", (Pow (FInt 1))))))
          ))) |}]
;;

let%expect_test _ =
  parsed_result "7.0<m/sec*dm> + 7.0<sp>" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Add),
       (EApp (
          (EConst
             (Measure_float ((FFloat 7.),
                (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
                   (MMeasure ((SMeasure ("sec", (Pow (FInt 1)))), Mul,
                      (SMeasure ("dm", (Pow (FInt 1))))))
                   ))
                ))),
          (EConst
             (Measure_float ((FFloat 7.), (SMeasure ("sp", (Pow (FInt 1)))))))
          ))
       )) |}]
;;

let%expect_test _ =
  parsed_result
    "[<Measure>] type hz = m / sec * sm * luck / unluck"
    parse_expression
    show_expression;
  [%expect
    {|
    (EMeasure
       (MMeasure_init ((SMeasure ("hz", (Pow (FInt 1)))),
          (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
             (MMeasure ((SMeasure ("sec", (Pow (FInt 1)))), Mul,
                (MMeasure ((SMeasure ("sm", (Pow (FInt 1)))), Mul,
                   (MMeasure ((SMeasure ("luck", (Pow (FInt 1)))), Div,
                      (SMeasure ("unluck", (Pow (FInt 1))))))
                   ))
                ))
             ))
          ))) |}]
;;

let%expect_test _ =
  parsed_result "let x = 1.<m>" parse_expression show_expression;
  [%expect
    {|
    (ELet ("NotRec", "x",
       (EConst (Measure_float ((FFloat 1.), (SMeasure ("m", (Pow (FInt 1))))))))) |}]
;;

let%expect_test _ =
  parsed_result "let x = 1.0<m / sm * dm / sec * damn>" parse_expression show_expression;
  [%expect
    {|
    (ELet ("NotRec", "x",
       (EConst
          (Measure_float ((FFloat 1.),
             (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
                (MMeasure ((SMeasure ("sm", (Pow (FInt 1)))), Mul,
                   (MMeasure ((SMeasure ("dm", (Pow (FInt 1)))), Div,
                      (MMeasure ((SMeasure ("sec", (Pow (FInt 1)))), Mul,
                         (SMeasure ("damn", (Pow (FInt 1))))))
                      ))
                   ))
                ))
             )))
       )) |}]
;;

let%expect_test _ =
  parsed_result "7.77<m/cm> + 7.73<m/cm>" parse_expression show_expression;
  [%expect
    {|
    (EApp ((EBinaryOp Add),
       (EApp (
          (EConst
             (Measure_float ((FFloat 7.77),
                (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
                   (SMeasure ("cm", (Pow (FInt 1))))))
                ))),
          (EConst
             (Measure_float ((FFloat 7.73),
                (MMeasure ((SMeasure ("m", (Pow (FInt 1)))), Div,
                   (SMeasure ("cm", (Pow (FInt 1))))))
                )))
          ))
       )) |}]
;;

(** Factorial test *)

let%expect_test _ =
  parsed_result
    "let rec fact n = if n = 1 then 1 else n * fact (n - 1)"
    parse_expression
    show_expression;
  [%expect
    {|
  (ELet ("Rec", "fact",
     (EFun ((PVar "n"),
        (EIfElse (
           (EApp ((EBinaryOp Eq), (EApp ((EVar "n"), (EConst (FInt 1)))))),
           (EConst (FInt 1)),
           (EApp ((EBinaryOp Mul),
              (EApp ((EVar "n"),
                 (EApp ((EVar "fact"),
                    (EApp ((EBinaryOp Sub),
                       (EApp ((EVar "n"), (EConst (FInt 1))))))
                    ))
                 ))
              ))
           ))
        ))
     ))
     |}]
;;

let%expect_test _ =
  parsed_result "fact 6" parse_expression show_expression;
  [%expect {| (EApp ((EVar "fact"), (EConst (FInt 6)))) |}]
;;

(** Fibonacci test*)

let%expect_test _ =
  parsed_result
    "let rec fib n = if n <= 1 then n else fib(n - 1) + fib(n - 2)"
    parse_expression
    show_expression;
  [%expect
    {|
    (ELet ("Rec", "fib",
       (EFun ((PVar "n"),
          (EIfElse (
             (EApp ((EBinaryOp Leq), (EApp ((EVar "n"), (EConst (FInt 1)))))),
             (EVar "n"),
             (EApp ((EBinaryOp Add),
                (EApp (
                   (EApp ((EVar "fib"),
                      (EApp ((EBinaryOp Sub),
                         (EApp ((EVar "n"), (EConst (FInt 1))))))
                      )),
                   (EApp ((EVar "fib"),
                      (EApp ((EBinaryOp Sub),
                         (EApp ((EVar "n"), (EConst (FInt 2))))))
                      ))
                   ))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  parsed_result "fib 6" parse_expression show_expression;
  [%expect {| (EApp ((EVar "fib"), (EConst (FInt 6)))) |}]
;;
