(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  interpret
    {| effect NotDigit: char -> int effect

    let int_of_char c = match c with
    | '0' -> 0
    | '1' -> 1 
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | c -> perform (NotDigit c)
    

let rec sum_up li = match li with
| [] -> 0
| h :: tl -> int_of_char h + sum_up tl


let test_l = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']

let res = try sum_up test_l with
| (NotDigit x) k -> continue k 0
|};
  [%expect
    {|
    val NotDigit : char -> int effect = <effect>
    val int_of_char : char -> int = <fun>
    val sum_up : char list -> int = <fun>
    val test_l : char list = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']
    val res : int = 25 |}]
;;

let%expect_test _ =
  interpret
    {| 
      effect E : char -> int effect ;;
      effect G : int -> int effect ;;

      let f = 
        try perform E 'a' with
        | (G x) k -> continue k (x / 5)

    |};
  [%expect {|
    Error: no suitable handler was found for effect 'E'. |}]
;;

let%expect_test _ =
  interpret
    {| 
      effect E : char -> int effect ;;
      effect G : int -> int effect ;;

      let f x = 
        try perform x with
        | (E x) k -> match x with | '0' -> continue k 0 | '1' -> continue k 1 | _ -> continue k 2
        | (G x) k -> continue k (x / 5)
      ;;
      
      let res1 = f (E '0')
      let res2 = f (E '1')
      let res3 = f (E '9')
      let res4 = f (G 26)

    |};
  [%expect
    {|
    val E : char -> int effect = <effect>
    val G : int -> int effect = <effect>
    val f : 'a effect -> 'a = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 2
    val res4 : int = 5 |}]
;;

let%expect_test _ =
  interpret
    {| 
      effect E : char -> int effect ;;
      let k = 4 ;;

      let res = try perform E 'a' with
      | (E x) l -> continue k x
      ;;

    |};
  [%expect {|
    Type error: variable 'k' is not continue variable. |}]
;;

let%expect_test _ =
  interpret
    {| 
      effect E : int effect ;;

      let f = try perform E with
      | E k -> continue k 0

    |};
  [%expect {|
    val E : int effect = <effect>
    val f : int = 0 |}]
;;
