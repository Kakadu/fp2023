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
  pi "return 2 * 'string'";
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
  pi "return 2 == 2";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 2 == 3";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return true == 1";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null == 'test'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return null < 'test'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 0 < 'test'";
  [%expect {| Programm return: false |}]
;;

(*not_equal*)

let%expect_test _ =
  pi "return 2!= 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 2!= 2";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return true!= 1";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return null!= 'test'";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null != 0";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 'test'!= 'test'";
  [%expect {| Programm return: false |}]
;;

(*strict equal*)
let%expect_test _ =
  pi "return 4 === 4";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 'foo' === 'foo'";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return true === 'true'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return true === 'true'";
  [%expect {| Programm return: false |}]
;;

(*strict not_equal*)
let%expect_test _ =
  pi "return 4 !== 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 'foo' !== 'foo'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return true !== false";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return true !== 'true'";
  [%expect {| Programm return: true |}]
;;

(**)

(*greater*)
let%expect_test _ =
  pi "return 4 > 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 4 > 4";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 4 > '4'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return '4' > 4";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return \"aa\" > \"ab\"";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return null > 0";
  [%expect {| Programm return: false |}]
;;

(*greater_equal*)
let%expect_test _ =
  pi "return 4 >= 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 4 >= 4";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return 4 >= '4'";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return '4' >= 4";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return \"aa\" >= \"ab\"";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return null >= 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null >= 'test'";
  [%expect {| Programm return: false |}]
;;

(*less*)
let%expect_test _ =
  pi "return 4 < 3";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 4 < 4";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 4 < '4'";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return '4' < 4";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return \"aa\" < \"ab\"";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null < 0";
  [%expect {| Programm return: false |}]
;;

(*greater_equal*)
let%expect_test _ =
  pi "return 4 <= 3";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  pi "return 4 <= 4";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return \"aa\" <= \"ab\"";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null <= 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  pi "return null <= 'test'";
  [%expect {| Programm return: false |}]
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

let%expect_test _ =
  pi "let a = b; function b() {return 10;}; return a()";
  [%expect {| Programm return: 10 |}]
;;

let%expect_test _ =
  pi "let a = 4; function b() {let a = 8; return a;}; return b()";
  [%expect {| Programm return: 8 |}]
;;

(*anon function*)

let%expect_test _ =
  pi "let a = function (b){\n    return b;\n  }; return a(4)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "let a = function (b){\n    return b;\n  }; return a";
  [%expect {| Programm return: [Function: a] |}]
;;

let%expect_test _ =
  pi "return function (b){\n    return b;\n  }(4)";
  [%expect {| Programm return: 4 |}]
;;

(*arrow function*)
let%expect_test _ =
  pi "return ((b) => b)(4)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "return ((b) => {return b})(4)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "let a = (b) => {return b}; return a(4)";
  [%expect {| Programm return: 4 |}]
;;

let%expect_test _ =
  pi "let a = (b) => {return b}; return a";
  [%expect {| Programm return: [Function: a] |}]
;;

(*objects*)

let%expect_test _ =
  pi "let a = { lang : \"Ocaml\", num : 5}; return a";
  [%expect {| Programm return: { lang: 'Ocaml', num: 5 } |}]
;;

let%expect_test _ =
  pi "let a = { lang : \"Ocaml\", num : 5}; return a.lang";
  [%expect {| Programm return: Ocaml |}]
;;

let%expect_test _ =
  pi "let a = { lang : \"Ocaml\", num : 5}; return a[\"nu\"+\"m\"]";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "let a = { lang : \"Ocaml\", [\"nu\"+\"m\"] : 5+5}; return a.num";
  [%expect {| Programm return: 10 |}]
;;

let%expect_test _ =
  pi "let a = { lang : \"Ocaml\", num : 5}; return a.ab";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  pi "let a = { get_num : (b) => b}; return a.get_num(5)";
  [%expect {| Programm return: 5 |}]
;;

let%expect_test _ =
  pi "let a = { sayHi() {\n    return \"Hi!\"\n  }}; return a.sayHi()";
  [%expect {| Programm return: Hi! |}]
;;
