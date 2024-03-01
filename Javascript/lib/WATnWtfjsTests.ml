(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Print

(**--------------------Tests from WAT--------------------*)

let%expect_test _ =
  print_return "return [] + []";
  [%expect {| Programm return:  |}]
;;

let%expect_test _ =
  print_return "return [] + {}";
  [%expect {| Programm return: [object Object] |}]
;;

(*In WAT answer is 0, because of it's in context of statements, so {} is not an object, is a block*)
(*but in this interpreter operators can only be in expression so answer is [object Object]*)
let%expect_test _ =
  print_return "return {} + []";
  [%expect {| Programm return: [object Object] |}]
;;

(*analogical*)
let%expect_test _ =
  print_return "return {} + {}";
  [%expect {| Programm return: [object Object][object Object] |}]
;;

(*cases:
  Array(16)
  Array(16).join("wat")
  Array(16).join("wat" + 1)
  Array(16).join("wat" - 1) + " Batman!"
  are missed because array constructor is not supported
  but behaviour can be recreated by other methods:*)

(*Array(16)*)
let%expect_test _ =
  print_return "let a = []; a[6]=undefined; return a+\"\"";
  [%expect {| Programm return: ,,,,,,undefined  |}]
;;

(*Array(16).join("wat")*)
let%expect_test _ =
  print_return "let a = []; a[6]=\"wat\"; a[4]=\"wat\"; return a+\"\"";
  [%expect {| Programm return: ,,,,wat,,wat |}]
;;

(*Array(16).join("wat" + 1)*)
let%expect_test _ =
  print_return "let a = []; a[6]=\"wat\"; a[4]=\"wat\"+1; return a+\"\"";
  [%expect {| Programm return: ,,,,wat1,,wat |}]
;;

(*Array(16).join("wat" - 1) + " Batman!"*)
let%expect_test _ =
  print_return "let a = []; a[6]=\"wat\"; a[4]=\"wat\"-1; return a+\"\"";
  [%expect {| Programm return: ,,,,NaN,,wat |}]
;;

(**-------------------Tests from wtfjs-------------------*)

(*What's not supported:
  - Object.is is not supported
  - get string char by index is not supported
    tests:
      (![] + [])[+[]] +
      (![] + [])[+!+[]] +
      ([![]] + [][[]])[+!+[] + [+[]]] +
      (![] + [])[!+[] + !+[]];
      const c = "constructor"; c[c][c]('console.log("WTF?")')();
  - instanceof, class, extends not supported
    tests:
      class Foo extends null {};return new Foo() instanceof null;
      return null instance of Object
  - length is not supported
    test:
      let a = [, , ,];\na.length;\nreturn a.toString();
  - Some cases of array equality don't work properly and were deleted
  - Number constructor isn't implemented now
    tests:
      let a = Number(); return a
      let a = Number(undefined); return a
*)

let%expect_test _ =
  print_return "return [] == ![]";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return (true == []) || (true == ![])";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return (!!\"false\" == !!\"true\") && (!!\"false\" === !!\"true\")";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return 'b' + 'a' + +'a' + 'a'";
  [%expect {| Programm return: baNaNa |}]
;;

let%expect_test _ =
  print_return "return NaN === NaN";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return -0 === 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return !![]";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [] == true";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return !!null";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return null == false";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return [1, 2, 3] + [4, 5, 6]";
  [%expect {| Programm return: 1,2,34,5,6 |}]
;;

let%expect_test _ =
  print_return "return [] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [''] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [0] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [0] == ''";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return [''] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [null] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[]] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[]] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[[[[[]]]]]] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[[[[[]]]]]] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return true + true";
  [%expect {| Programm return: 2 |}]
;;

let%expect_test _ =
  print_return "return typeof NaN";
  [%expect {| Programm return: number |}]
;;

let%expect_test _ =
  print_return "return typeof []";
  [%expect {| Programm return: object |}]
;;

let%expect_test _ =
  print_return "return typeof null";
  [%expect {| Programm return: object |}]
;;

let%expect_test _ =
  print_return "return 0.1 + 0.2 === 0.3";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return 1 < 2 < 3";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return 3 > 2 > 1";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return '222' - -'111'";
  [%expect {| Programm return: 333 |}]
;;

let%expect_test _ =
  print_return "return [4] * [4]";
  [%expect {| Programm return: 16 |}]
;;

let%expect_test _ =
  print_return "return [] * []";
  [%expect {| Programm return: 0 |}]
;;

let%expect_test _ =
  print_return "return [4, 4] * [4, 4]";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  print_return "{}{}";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  print_return "{}{}{}";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  print_return "\n  function a(x) {\n    console.log(x);\n  }\n  return a();";
  [%expect {| Programm return: undefined|}]
;;
