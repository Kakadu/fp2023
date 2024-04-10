(** Copyright 2023-2024, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpret

let parse_and_interpret str =
  match Parser.parse str with
  | Ok parse_result ->
    (match InterpretResult.exec_program parse_result with
     | Ok actual -> Format.printf "%a" pp_env actual
     | Error err -> Format.printf "%a" print_failure err)
  | Error err -> Format.printf "Parsing error: %s\n " err
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 5" in
  [%expect {|x: 5 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 5 < 10" in
  [%expect {|x: true |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 5 + 5" in
  [%expect {|x: 10 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 231 * 765 + 5" in
  [%expect {|x: 177870 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 5" in
  [%expect {|x: 5 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = (false || true) && true" in
  [%expect {|x: true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = fun x -> x * 2 " in
  [%expect {|x: <fun>|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = 2 / 2" in
  [%expect {|x: 1|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let x = if 1+1=2 then 1 else 0" in
  [%expect {|x: 1|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let a = fun x -> x\n      let b = a 5 " in
  [%expect {|
    a: <fun>
    b: 5|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let rec fac = fun n -> if n < 2 then 1 else n * fac (n-1)\n  let sdf = fac 5"
  in
  [%expect {|
fac: <rec fun>
sdf: 120|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let fff = (1, 2, 3)" in
  [%expect {|
fff: (1, 2, 3)|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let mtch = fun x -> match x with | 1 -> \"one\" | 2 -> \"two\" | _ -> \"other\"\n\
      \      let a = mtch 1"
  in
  [%expect {|
a: one
mtch: <fun>|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let input = fun x -> match x with | 1 -> (1, 2, 3) | 2 -> (4,5,6) | _ -> \"other\"\n\
      \      let a = input 2"
  in
  [%expect {|
a: (4, 5, 6)
input: <fun>|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let d = fun a -> a * 5\n  let b = z 5" in
  [%expect {|
Unknow Variable: "z"|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let d = 5 / 0" in
  [%expect {|
Division by zero|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let summ = fun x -> x + 8\n  let res = summ 8" in
  [%expect {|
res: 16
summ: <fun>|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let summ = fun (x,y) -> x + y\n  let res = summ 1" in
  [%expect {|
Error in pattern matching|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let summ = fun (x,y) -> x + y\n\
      \  let del = fun (x,y) -> x / y\n\
      \  let res = summ (1,2)\n\
      \  let resdel = del(5,5)"
  in
  [%expect {|
del: <fun>
res: 3
resdel: 1
summ: <fun>|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret "let s = \"string\"" in
  [%expect {|
s: string|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let s = \"string\"\n  let checkstring = if s = \"string\" then true else false "
  in
  [%expect {|
checkstring: true
s: string|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let binop = fun (a, b, c) -> a + b / c \n  let res = binop(2, 6, 3)"
  in
  [%expect {|
binop: <fun>
res: 2|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let rec fix = fun f -> (fun x -> f (fix f) x)\n\
      \  let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))\n\
      \  let a = fac 6"
  in
  [%expect {|
a: 720
fac: <fun>
fix: <rec fun>|}]
;;

(* let%expect_test _ =
   let _ = parse_and_interpret "let x = fun (y,a) -> y + a\n      let res = x(5,5)" in
   [%expect {|
a: 720
fac: <fun>
fix: <rec fun>|}]
   ;; *)

let%expect_test _ =
  let _ =
    parse_and_interpret
      "let rec fix = (fun f -> (fun x -> f (fix f) x))\n\
       let fac_cps = fix (fun self -> (fun (n, k) -> if n <= 1 then k 1 else self ((n - \
       1), (fun x -> k (n * x)))))\n\
       let a = fac_cps (6, (fun x -> x))"
  in
  [%expect {|
    a: 720
    fac_cps: <fun>
    fix: <rec fun> |}]
;;
