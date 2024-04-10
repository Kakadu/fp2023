(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Without effects *)

let%expect_test _ =
  inference
    {|
    let g x = if x = 0 || x = 1 then true else false ;;

    let f x =
      let rec helper x =
        match x with
        | [] -> true
        | hd :: tl ->
          (g hd) && helper tl
      in
      helper x
    ;;
  |};
  [%expect {|
    val g : int -> bool
    val f : int list -> bool |}]
;;

(* ---------------- *)

(* With effects *)

let%expect_test _ =
  inference
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
    val NotDigit : char -> int effect
    val int_of_char : char -> int
    val sum_up : char list -> int
    val test_l : char list
    val res : int |}]
;;

(* ---------------- *)
