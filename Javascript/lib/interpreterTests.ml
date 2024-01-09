(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Print

(**---------------Expressions tests---------------*)

let%expect_test _ =
  pi "return 4";
  [%expect {| Programm return: 4 |}]
;;

(*number plus*)
let%expect_test _ =
  pi "return 4+5";
  [%expect {| Programm return: 9 |}]
;;

let%expect_test _ =
  pi "return 4+5+6";
  [%expect {| Programm return: 15 |}]
;;

let%expect_test _ =
  pi "return 4+null";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "return 4+undefined";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  pi "return 4+true";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "return false+4.5";
  [%expect {| Programm return: 4.5 |}]
;;

(*string plus*)
let%expect_test _ =
  pi "return 4+\"5\"";
  [%expect {| Programm return: 45 |}]
;;

let%expect_test _ =
  pi "return \"\"+true";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return \"\"+null";
  [%expect {| Programm return: null |}]
;;

let%expect_test _ =
  pi "return \"\"+undefined";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  pi "return \"\"+\"str\"";
  [%expect {| Programm return: str |}]
;;

(*subtract mixed*)
let%expect_test _ =
  pi "return 2-1";
  [%expect {| Programm return: 1 |}]
;;

let%expect_test _ =
  pi "return 1-2";
  [%expect {| Programm return: -1 |}]
;;

let%expect_test _ =
  pi "return 2-'1'";
  [%expect {| Programm return: 1 |}]
;;

let%expect_test _ =
  pi "return '2'-1";
  [%expect {| Programm return: 1 |}]
;;

let%expect_test _ =
  pi "return 10 -true";
  [%expect {| Programm return: 9 |}]
;;

let%expect_test _ =
  pi "return 10 - 'true'";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  pi "return 10- false";
  [%expect {| Programm return: 10 |}]
;;

let%expect_test _ =
  pi "return true -  11";
  [%expect {| Programm return: -10 |}]
;;

(*multiplication*)
let%expect_test _ =
  pi "return 3*4";
  [%expect {| Programm return: 12 |}]
;;

let%expect_test _ =
  pi "return -3*4";
  [%expect {| Programm return: -12 |}]
;;

let%expect_test _ =
  pi "return '3' *'2'";
  [%expect {| Programm return: 6 |}]
;;

let%expect_test _ =
  pi "return '3' * 2";
  [%expect {| Programm return: 6 |}]
;;

let%expect_test _ =
  pi "return 3 * '2'";
  [%expect {| Programm return: 6 |}]
;;

let%expect_test _ =
  pi "return 'string' * 'string'";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  pi "return 2 * \"string\"";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  pi "return true * 5";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "return false * 5";
  [%expect {| Programm return: 0 |}]
;;

(*division*)
let%expect_test _ =
  pi "return 12 / 2";
  [%expect {| Programm return: 6 |}]
;;

let%expect_test _ =
  pi "return 3 / 2";
  [%expect {| Programm return: 1.5 |}]
;;

let%expect_test _ =
  pi "return 6 / '3'";
  [%expect {| Programm return: 2 |}]
;;

let%expect_test _ =
  pi "return 2 / 0";
  [%expect {| Programm return: Infinity |}]
;;

let%expect_test _ =
  pi "return 2 / 'foo'";
  [%expect {| Programm return: NaN |}]
;;

(*equal*)
let%expect_test _ =
  pi "return 4 == 4";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 'foo' == 'foo'";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return true == 'true'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return '1' == 1";
  [%expect {| Programm return: true |}]

let%expect_test _ =
  pi "return '0' == false";
  [%expect {| Programm return: true |}]

let%expect_test _ =
  pi "return '1' == false";
  [%expect {| Programm return: true |}]

(*not_equal*)
let%expect_test _ =
  pi "return 4 != 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 'foo' != 'foo'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return true != false";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return true != 'true'";
  [%expect {| Programm return: true |}]
;;

(*unary operators*)
let%expect_test _ =
  pi "return +4";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "return -4";
  [%expect {| Programm return: -4 |}]
;;

let%expect_test _ =
  pi "return -(-4)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "return -\"j\"";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  pi "return +\"j\"";
  [%expect {| Programm return: NaN |}]
;;

(*infinity*)
let%expect_test _ =
  pi "return Infinity";
  [%expect {| Programm return: Infinity |}]
;;

let%expect_test _ =
  pi "return -Infinity";
  [%expect {| Programm return: -Infinity |}]
;;

(**---------------Var tests---------------*)

let%expect_test _ =
  pi "let a = 4; return a";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "let a = 4+5; return a";
  [%expect {| Programm return: 9 |}]
;;

let%expect_test _ =
  pi "let a = 4; let a = 5; return a";
  [%expect
    {| Error: Interpreter error > SyntaxError: Identifier 'a' has already been declared |}]
;;

let%expect_test _ =
  pi "return a";
  [%expect
    {| Error: Interpreter error > error in return expression > ReferenceError: Cannot access 'a' before initialization |}]
;;

let%expect_test _ =
  pi "let a; return a";
  [%expect {| Programm return: undefined |}]
;;

(*Block tests*)
let%expect_test _ =
  pi "{ let a; }";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  pi "{ let a = 4; } return a";
  [%expect
    {| Error: Interpreter error > error in return expression > ReferenceError: Cannot access 'a' before initialization |}]
;;

let%expect_test _ =
  pi "{ let a = 4; return a} ";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "let a = 4; { let a = 5; return a} ";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "let a = 4; { return a} ";
  [%expect {| Programm return: 4 |}]
;;
