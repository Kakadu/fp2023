open FSharpUnitsOfMeasure_lib
open Ast
open Parser

let parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "%s" e
;;

(** Types test*)

let%expect_test _ =
  parsed_result "777" parse_types show_types;
  [%expect {| (FInt 777) |}]
;;

let%expect_test _ =
  parsed_result "-777" parse_types show_types;
  [%expect {| (FInt -777) |}]
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

(** Factorial test *)

let%expect_test _ =
  parsed_result "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" parse_expression show_expression;
  [%expect
  {|
  (ELetRec ("fact",
     (EFun ((EVar "n"),
        (EIfElse (
           (EApp ((EBinaryOp Eq), (EApp ((EVar "n"), (ETypes (FInt 1)))))),
           (ETypes (FInt 1)),
           (EApp ((EBinaryOp Mul),
              (EApp ((EVar "n"),
                 (EApp ((EVar "fact"),
                    (EApp ((EBinaryOp Sub),
                       (EApp ((EVar "n"), (ETypes (FInt 1))))))
                    ))
                 ))
              ))
           ))
        ))
     )) |}]
;;
