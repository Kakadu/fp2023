(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List construction *)

let%expect_test _ =
  inference {| 
      5 :: 6
    |};
  [%expect
    {| Type error: unification failed - type int does not match expected type int list |}]
;;

let%expect_test _ =
  inference {| 
      let x1 = 5 :: [6]
      let x2 = 5 :: 6 :: []
    |};
  [%expect {|
    val x1 : int list
    val x2 : int list |}]
;;

let%expect_test _ =
  inference {| 
      5 :: 'c'
    |};
  [%expect
    {| Type error: unification failed - type char does not match expected type int list |}]
;;

(* ---------------- *)

(* List *)

let%expect_test _ =
  inference {| let x = ["Hello"; "World"] |};
  [%expect {| val x : string list |}]
;;

let%expect_test _ =
  inference {| let x = [[('a', 1); ('b', 5)]; [('c', 23); ('d', 11)]] |};
  [%expect {| val x : char * int list list |}]
;;

let%expect_test _ =
  inference {| let x = [fun x -> x + 1 ; fun x -> x + 2 ; fun x -> x + 3] |};
  [%expect {| val x : (int -> int) list |}]
;;

(* ---------------- *)

(* Tuple *)

let%expect_test _ =
  inference {| let x = (true, "Yes", 25 * 3) |};
  [%expect {| val x : bool * string * int |}]
;;

let%expect_test _ =
  inference {| 
      (5, 'c', "aaa", true, ())
    |};
  [%expect {| - : int * char * string * bool * unit |}]
;;

let%expect_test _ =
  inference {| 
      (1, (1,'a'), "name", 'a', true :: false :: [])
    |};
  [%expect {| - : int * (int * char) * string * char * bool list |}]
;;

(* ---------------- *)
