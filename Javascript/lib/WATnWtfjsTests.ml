(** Copyright 2023, Kuarni, AlexShmak *)

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

let%expect_test _ =
  print_return "return {} + []";
  [%expect {| Programm return: 0 |}]
;;

let%expect_test _ =
  print_return "return {} + {}";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  print_return "return Array(16)";
  [%expect {| Programm return: ,,,,,,,,,,,,,,,, |}]
;;

let%expect_test _ =
  print_return "return Array(16).join(\"wat\")";
  [%expect {| Programm return: watwatwatwatwatwatwatwatwatwatwatwatwatwatwatwat |}]
;;

let%expect_test _ =
  print_return "return Array(16).join(\"wat\" + 1)";
  [%expect
    {| Programm return: wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1wat1 |}]
;;

let%expect_test _ =
  print_return "return Array(16).join(\"wat\" - 1) + \" Batman!\"";
  [%expect
    {| Programm return: NaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaNNaN Batman! |}]
;;

(**-------------------Tests from wtfjs-------------------*)

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
  print_return "return Object.is(NaN, NaN)";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return Object.is(-0, 0)";
  [%expect {| Programm return: false |}]
;;

let%expect_test _ =
  print_return "return -0 === 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return
    "return (![] + [])[+[]] +\n\
    \  (![] + [])[+!+[]] +\n\
    \  ([![]] + [][[]])[+!+[] + [+[]]] +\n\
    \  (![] + [])[!+[] + !+[]]";
  [%expect {| Programm return: 'fail' |}]
;;

let%expect_test _ =
  print_return "return !![]";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [] == true]";
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
  print_return "class Foo extends null {};return new Foo() instanceof null;";
  [%expect {| Programm return: TypeError: function is not a function |}]
;;

let%expect_test _ =
  print_return "return [1, 2, 3] + [4, 5, 6]";
  [%expect {| Programm return: '1,2,34,5,6' |}]
;;

let%expect_test _ =
  print_return "\n  let a = [, , ,];\na.length;\nreturn a.toString(); ";
  [%expect {| Programm return: ',,' |}]
;;

let%expect_test _ =
  print_return "return [1, 2, 3] + [4, 5, 6]";
  [%expect {| Programm return: '1,2,34,5,6' |}]
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
  print_return "return [null] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [null] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [undefined] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [undefined] == 0";
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
  print_return "return [[[[[[ null ]]]]]] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[[[[[ null ]]]]]] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[[[[[ undefined ]]]]]] == 0";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "return [[[[[[ undefined ]]]]]] == ''";
  [%expect {| Programm return: true |}]
;;

let%expect_test _ =
  print_return "let a = Number(); return a";
  [%expect {| Programm return: 0 |}]
;;

let%expect_test _ =
  print_return "let a = Number(undefined); return a";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  print_return "return true + true";
  [%expect {| Programm return: 2 |}]
;;

let%expect_test _ =
  print_return "return typeof NaN";
  [%expect {| Programm return: 'number' |}]
;;

let%expect_test _ =
  print_return "return typeof []";
  [%expect {| Programm return: 'object' |}]
;;

let%expect_test _ =
  print_return "return typeof null";
  [%expect {| Programm return: 'object' |}]
;;

let%expect_test _ =
  print_return "return null instance of Object";
  [%expect {| Programm return: false |}]
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
  print_output "const c = \"constructor\"; c[c][c]('console.log(\"WTF?\")')();";
  [%expect {| Programm return: NaN |}]
;;

let%expect_test _ =
  print_return "return {}{}";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  print_return "return {}{}{}";
  [%expect {| Programm return: undefined |}]
;;

let%expect_test _ =
  print_return "return {foo: 'bar'}{}";
  [%expect {| Programm return: bar |}]
;;

let%expect_test _ =
  print_return "return {}{foo: 'bar'}";
  [%expect {| Programm return: bar |}]
;;

let%expect_test _ =
  print_return "return {}{foo: 'bar'}{}";
  [%expect {| Programm return: bar |}]
;;

let%expect_test _ =
  print_return "return {a: 'b'}{c:' d'}{}";
  [%expect {| Programm return: d |}]
;;

let%expect_test _ =
  print_return "return {a: 'b', c:' d'}{}";
  [%expect {| Programm return: SyntaxError: Unexpected token ':'|}]
;;

let%expect_test _ =
  print_return "return ({}{})";
  [%expect {| Programm return: SyntaxError: Unexpected token '{}'|}]
;;

let%expect_test _ =
  print_return
    "\n\
    \  function a(x) {\n\
    \    arguments[0] = \"hello\";\n\
    \    console.log(x);\n\
    \  }\n\
    \  return a();";
  [%expect {| Programm return: undefined|}]
;;
