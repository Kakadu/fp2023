(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Const tests *)
let%expect_test _ =
  inference {| let x = 1 |};
  [%expect {|
    val x : int |}]
;;

let%expect_test _ =
  inference {| let x = 'a' |};
  [%expect {|
    val x : char |}]
;;

let%expect_test _ =
  inference {| let x = "string" |};
  [%expect {|
    val x : string |}]
;;

let%expect_test _ =
  inference {| let x = true |};
  [%expect {|
    val x : bool |}]
;;

let%expect_test _ =
  inference {| let x = ()|};
  [%expect {|
    val x : unit |}]
;;

(* ---------------- *)

(* Binary, unary operations *)

let%expect_test _ =
  inference {| 
      1 + 1
    |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| 
      'a' = 'b'
    |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference {| 
      5 + 'c'
    |};
  [%expect
    {| Type error: unification failed - type char does not match expected type int |}]
;;

let%expect_test _ =
  inference {| 
      let f x y = x - 1 < y || x > y
    |};
  [%expect {| val f : int -> int -> bool |}]
;;

let%expect_test _ =
  inference {| 
      not true && false
    |};
  [%expect {| - : bool |}]
;;

(* ---------------- *)

(* If Then Else *)

let%expect_test _ =
  inference {| 
      let x = 5

      let res = if x > 1 then true else false

    |};
  [%expect {|
    val x : int
    val res : bool |}]
;;

let%expect_test _ =
  inference {| 
      let x = 5

      let res = if x > 1 then true else 1

    |};
  [%expect
    {|
    Type error: unification failed - type bool does not match expected type int |}]
;;

(* ---------------- *)
