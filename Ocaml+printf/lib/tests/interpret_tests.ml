(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

(* for tests assumed that parsing and type inference was succsessful *)
let eval_expr_and_print str =
  let res = Result.get_ok (Parser.run_parser_expr str) in
  let _, res = Result.get_ok (Inferencer.run_infer_expr res) in
  let res = Interpreter.run_eval_expr res in
  match res with
  | Base.Result.Ok v -> Format.printf "%a" Interpreter.pp_value v
  | Base.Result.Error err -> Format.printf "%a" Interpreter.pp_error err
;;

let eval_program_and_print str =
  let res = Result.get_ok (Parser.run_parser_program str) in
  let _, res = Result.get_ok (Inferencer.run_infer_program res) in
  let res = Interpreter.run_eval_program res in
  match res with
  | Base.Result.Ok v -> Format.printf "%a" Interpreter.EnvValues.pp_env_values v
  | Base.Result.Error err -> Format.printf "%a" Interpreter.pp_error err
;;

let%expect_test _ =
  let _ =
    eval_program_and_print {|
    let i = 0
    let str = "abc"
    let c = str.[i]
  |}
  in
  [%expect {|
    val c = 'a'
    val i = 0
    val str = "abc" |}]
;;

let%expect_test _ =
  let _ =
    eval_program_and_print
      {|
    let str = "1234567"
    let c = get str 1
    let b = false;;
    printf "string: %s; bool: %B\nnum: %d; char: %c\n" "abcdef" b 123 c
    let fmt1 = format_of_string "char2: %c; string: %s\n"
    let fmt2 = "char1: %c; " ^^ fmt1
    let fmt3 = format_of_string fmt2
    let my_printf = printf fmt3;;
    my_printf str.[(length str - 1)] c "str"
  |}
  in
  [%expect
    {|
    string: abcdef; bool: false
    num: 123; char: 2
    char1: 7; char2: 2; string: str
    val b = false
    val c = '2'
    val fmt1 = "char2: %c; string: %s\n" format
    val fmt2 = "char1: %c; char2: %c; string: %s\n" format
    val fmt3 = "char1: %c; char2: %c; string: %s\n" format
    val my_printf = <fun>
    val str = "1234567" |}]
;;

let%expect_test _ =
  let _ =
    eval_program_and_print
      {|
  let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  let a = fac 6
  let rev list =
    let rec helper acc list =
      match list with
      | [] -> acc
      | h :: tl -> helper (h :: acc) tl
    in
    helper [] list 
  let reversed1 = rev [1;2;3;4;5]
  let reversed2 = rev [true;false;false;false]
  |}
  in
  [%expect
    {|
    val a = 720
    val fac = <fun>
    val rev = <fun>
    val reversed1 = [5; 4; 3; 2; 1]
    val reversed2 = [false; false; false; true] |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let f = 2 in f|} in
  [%expect {| 2 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = 1 in let b = 2 in let c = 3 in a + b * c|} in
  [%expect {| 7 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let id x = x in let b a = id a in b 5|} in
  [%expect {| 5 |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print {|let rec f n = if n <= 1 then 1 else n * f (n - 1) in f 5|}
  in
  [%expect {| 120 |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let rec f a b = if a + b > 100 then a + b else f (a + 3) (b * 2) in f 1 5 |}
  in
  [%expect {| 176 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let rec x = 1 in x|} in
  [%expect {| 1 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let str = "abc" in str.[0]|} in
  [%expect {| 'a' |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = (1+3*4, 'c', "ab" ^ "cd", true) in a|} in
  [%expect {| (13, 'c', "abcd", true) |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let a = 1::2::[3;4] in match a with | h::tl -> (h, tl) | _ -> (1,[2])|}
  in
  [%expect {| (1, [2; 3; 4]) |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let str = "abc" in let a = (str, 'c') in match a with | ("abc", _) -> "yes" | _ -> "no"|}
  in
  [%expect {| "yes" |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let fst (a, b) = a in fst (2,3)|} in
  [%expect {| 2 |}]
;;

(* errors *)

let%expect_test _ =
  let _ = eval_expr_and_print {|let str = "a\nc" in str.[3]|} in
  [%expect {| Invalid argument: Index out of bounds |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = 0 in let b = 1 in b / a|} in
  [%expect {| Division by zero |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let f x = match x with | h::tl -> 1 in f []|} in
  [%expect {| Matching failure |}]
;;

(* I have two functions: "eval_program_and_print" and "eval_expr_and_print"
   In this test you wrote a declaration, so I replaced function *)
let%expect_test _ =
  let _ = eval_program_and_print {|let f x = format_of_string x|} in
  [%expect {| val f = <fun> |}]
;;
