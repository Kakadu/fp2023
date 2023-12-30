open FSharpUnitsOfMeasure_lib
open Ast
open Parser

let parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "%s" e
;;

(** Types test *)

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

(** Expressions with binary operations test *)

let%expect_test _ =
  parsed_result "1 + 2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Add), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1 - 2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Sub), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "z * v" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EVar "z"), (EVar "v"))))) |}]
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
  parsed_result "1     *    2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1     *    +2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "    1     *    +   2    " parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt 2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1     *   -2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt -2)))))) |}]
;;

let%expect_test _ =
  parsed_result "1     *   -         2" parse_expression show_expression;
  [%expect {| (EApp ((EBinaryOp Mul), (EApp ((EConst (FInt 1)), (EConst (FInt -2)))))) |}]
;;

let%expect_test _ =
  parsed_result "     1     *   -         2      " parse_expression show_expression;
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

(** Factorial test *)

let%expect_test _ =
  parsed_result "let rec fact n = if n = 1 then 1 else n * (fact ( n - 1 ))" parse_expression show_expression;
  [%expect
  {|
  (ELetRec ("fact",
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

