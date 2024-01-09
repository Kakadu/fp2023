(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib
open Format

let parse_and_interpret_result str =
  match Parser.parse str with
  | Ok parse_result ->
    (match Interpreter.InterpreterResult.exec_program parse_result with
     | Ok actual -> printf "%a" InterpreterTypes.pp_env actual
     | Error err -> printf "%a" InterpreterTypes.pp_failure err)
  | Error err -> printf "Parsing error: %s\n" err
;;

(* tests *)
let%expect_test _ =
  let _ = parse_and_interpret_result "let n = 5" in
  [%expect {| "n": 5 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let x : bool = (false || true) && true" in
  [%expect {| "x": true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let x : bool = false && true" in
  [%expect {| "x": false|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let x : bool = true || true" in
  [%expect {| "x": true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let x : bool = true == true" in
  [%expect {| "x": true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let x : bool = true <> false" in
  [%expect {| "x": true|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result "let n = fun x -> 8 == x + 6 * 2 / 3\n    let a = n 4"
  in
  [%expect {|
    "a": true
    "n": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let n = \"a\"\n    let a =  \"a\" == n \n let b = \"b\" <> n"
  in
  [%expect {|
    "a": true
    "b": true
    "n": "a" |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * \
       factorial_recursive (n - 1)\n\
      \     let a = factorial_recursive 5\n\
      \     let b = factorial_recursive 6"
  in
  [%expect {|
    "a": 120
    "b": 720
    "factorial_recursive": <let rec> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let rec fix = fun f -> (fun x -> f (fix f) x)\n\n\
      \    let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))\n\n\
      \      let a = fac 5"
  in
  [%expect {|
    "a": 120
    "fac": <fun>
    "fix": <let rec> |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let rec n = 5" in
  [%expect {| "n": 5 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result "let n = fun y -> (let x = 5 in x + y)\n let f = n 7"
  in
  [%expect {|
    "f": 12
    "n": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result "let n = fun y -> if y > 7 then 5 else 3\n let f = n 7"
  in
  [%expect {|
    "f": 3
    "n": <fun>  |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result "let n = fun y -> if y > 7 then 5 else 3\n let f = n 100"
  in
  [%expect {|
    "f": 5
    "n": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | Tepa 4 -> true | _ -> false\n let n = a (Tepa 4)"
  in
  [%expect {|
    "a": <fun>
    "n": true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | Tepa 4 -> true | _ -> false\n let n = a (Tepa 5)"
  in
  [%expect {|
    "a": <fun>
    "n": false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | Tepa 4 -> true | _ -> false\n let n = a (NeTepa 5)"
  in
  [%expect {|
    "a": <fun>
    "n": false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let h = fun h :: tl -> h\n\
      \ let tl = fun h :: tl -> tl\n\
      \ let n = h (4 :: 5 :: 6)\n\
      \ let m = tl (4 :: 5 :: 6)"
  in
  [%expect {|
    "h": <fun>
    "m": [5; 6]
    "n": 4
    "tl": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let sum = fun (a, b) -> a + b\n\n\
      \ let sub = fun (a, b) -> a - b\n\
      \ let mul = fun (a, b) -> a * b\n\
      \ let n = sum (4, 5)\n\
      \ let m = sub (5, 6)\n\
      \ let k = mul (5, 8)"
  in
  [%expect
    {|
    "k": 40
    "m": -1
    "mul": <fun>
    "n": 9
    "sub": <fun>
    "sum": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let h = fun h :: tl -> h\n\
      \ let tl = fun h :: tl -> tl\n\
      \ let n = h (Tepa 46 :: Tepa 45 :: Tepa 44)\n\
      \ let m = tl (Tepa 46 :: Tepa 45 :: Tepa 44)"
  in
  [%expect
    {|
      "h": <fun>
      "m": ["Tepa" 45; "Tepa" 44]
      "n": "Tepa" 46
      "tl": <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | Tree (_, _) -> true | _ -> false\n\
      \ let n = a (Tree (Tree (Tree (2, 3), 2), Tree(2, 3)))"
  in
  [%expect {|
    "a": <fun>
    "n": true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | [] -> false | h :: tl -> true\n\
      \ let n = a (44 :: 45 :: 56)"
  in
  [%expect {|
    "a": <fun>
    "n": true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | [] -> false | h :: tl -> true\n let n = a []"
  in
  [%expect {|
    "a": <fun>
    "n": false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | true -> false | false -> true\n let n = a true"
  in
  [%expect {|
    "a": <fun>
    "n": false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      "let a = fun x -> match x with | \"A\" -> true | \"B\" -> true | _ -> false\n\
      \ let n = a \"C\""
  in
  [%expect {|
    "a": <fun>
    "n": false |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let n = 5 / 0" in
  [%expect {| DivisionByZeroError |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result "let sum = fun (a, b) -> a + b\n\n       let k = sum 8"
  in
  [%expect {| PatternMatchingError |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {|let n = 5 + "S"|} in
  [%expect {| ExecError: 5 # "S" |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result "let a = fun x -> x\n let n = b 5" in
  [%expect {|
    UnboundVariable: "b" |}]
;;
