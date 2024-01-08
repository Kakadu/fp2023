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

(*function test*)

let%expect_test _ =
  pi "function a() {return 5;}; return a()";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "function a(b) {return b;}; return a(7)";
  [%expect {| Programm return: 7 |}]
;;

let%expect_test _ =
  pi "function a(b, c) {return b+c;}; return a(7, 8)";
  [%expect {| Programm return: 15 |}]
;;

let%expect_test _ =
  pi "function a() {return 4;}; return a(7, 8)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "function a(j) {return j;}; return a()";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  pi "return a(); function a() {return 4;}";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "return a; function a() {}";
  [%expect {| Programm return: [Function: a] |}]
;;

let%expect_test _ =
  pi "let a = 6; function b() {return a;}; return b()";
  [%expect {| Programm return: 6 |}]
;;
