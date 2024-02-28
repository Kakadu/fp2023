(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Const patterns *)

let%expect_test _ =
  interpret
    {|
    let f = true

    let g = 
      match f with
      | true -> true
      | false -> false
  |};
  [%expect {|
    val f : bool = true
    val g : bool = true |}]
;;

let%expect_test _ =
  interpret
    {|
    let f = "str"

    let g = 
      match f with
      | "str" -> true
      | _ -> false
  |};
  [%expect {|
    val f : string = "str"
    val g : bool = true |}]
;;

let%expect_test _ =
  interpret {|
    let f = ()

    let g = match f with | () -> true
  |};
  [%expect {|
    val f : unit = ()
    val g : bool = true |}]
;;

(* ---------------- *)

(* List construction pattern *)

let%expect_test _ =
  interpret
    {|
    let f x = 
      (match x with
      | [] -> 0
      | hd :: tl -> hd)
    ;;

    let res1 = f []
    let res2 = f [1 ; 2 ; 3]
    let res3 = f (1 :: 2 :: 3 :: [])
  |};
  [%expect
    {|
    val f : int list -> int = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 1 |}]
;;

let%expect_test _ =
  interpret
    {|
    let f x = 
      (match x with
      | [] -> 0
      | hd :: snd :: tl -> hd)
    in
    f [1]
  |};
  [%expect
    {|
    Error: pattern matching failure - the value does not match any pattern. |}]
;;

(* ---------------- *)

(* List construction pattern *)

let%expect_test _ =
  interpret
    {|
    let f x = 
      (match x with
      | (x, 0) -> x
      | (x, 1) -> x + 1
      | _ -> 0)
    ;;

    let res1 = f (1, 2)
    let res2 = f (1, 0)
    let res3 = f (1, 1)
  |};
  [%expect
    {|
    val f : int * int -> int = <fun>
    val res1 : int = 0
    val res2 : int = 1
    val res3 : int = 2 |}]
;;

let%expect_test _ =
  interpret
    {|
    let f x = 
      (match x with
      | (x, 0) -> x
      | (x, 1) -> x + 1)
    in
    f (1, 2)
  |};
  [%expect
    {|
    Error: pattern matching failure - the value does not match any pattern. |}]
;;

(* ---------------- *)

(* Tuple pattern *)

let%expect_test _ =
  interpret
    {|
    let f x = 
      match x with
      | ((x,0), (z, 0)) -> x + z
    ;;

    let res = f ((5,0), (6, 1))
    |};
  [%expect
    {|
    Error: pattern matching failure - the value does not match any pattern. |}]
;;

let%expect_test _ =
  interpret
    {|
    let f x = 
      match x with
      | (x, y :: z) -> x + y
    ;;

    let res = f (1, [2])
    |};
  [%expect {|
    val f : int * int list -> int = <fun>
    val res : int = 3
     |}]
;;

let%expect_test _ =
  interpret
    {|
    let f x = 
      match x with
      | (x, y :: z :: q) -> x + y + z
    ;;

    let res = f (1, (1,2))
    |};
  [%expect
    {|
    Type error: unification failed - type int list does not match expected type int * int
     |}]
;;

(* ---------------- *)

(* Effect patterns *)

let%expect_test _ =
  interpret
    {|
    effect E : int effect
    effect G : int effect
    let f x = 
      match x with
      | E -> 0
      | G -> 0
    ;;

    let res1 = f E
    let res2 = f G
    |};
  [%expect
    {|
    val E : int effect = <effect>
    val G : int effect = <effect>
    val f : int effect -> int = <fun>
    val res1 : int = 0
    val res2 : int = 0 |}]
;;

let%expect_test _ =
  interpret
    {|
    effect E : int effect
    effect G : int effect
    let f x = 
      match x with
      | G -> 0
    ;;

    let res1 = f E
    ;;
    |};
  [%expect {| Error: pattern matching failure - the value does not match any pattern. |}]
;;

(* ---------------- *)
