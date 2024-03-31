(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DRozhkov_lib
open Inter

let%expect_test _ =
  run_inter {|
     let x = 5 + 6 - 3 * 7 / 1
    |};
  [%expect {|val x : int = -10|}]
;;

let%expect_test _ =
  run_inter
    {|
     let mymatch x =
        match x with
          | 1 -> true
          | _ -> false
 
      let result = mymatch 2
    |};
  [%expect {|
    val mymatch : int -> bool = <fun>
    val result : bool = false |}]
;;

let%expect_test _ =
  run_inter
    {|
     let mymatch x =
        match x with
          | hd :: tl -> hd
          | _ -> 0
      
     let a = mymatch [1; 2; 3]
    |};
  [%expect {|
    val a : int = 1
    val mymatch : int list -> int = <fun> |}]
;;

let%expect_test _ =
  run_inter {|
     let f a = if a < 1 then true else false

     let x = f 0
    |};
  [%expect {|
     val f : int -> bool = <fun>
     val x : bool = true |}]
;;

let%expect_test _ =
  run_inter
    {|
     let rec fact x = if x < 1 then 1 else x * fact (x - 1)

     let x = fact 5
    |};
  [%expect {|
    val fact : int -> int = <fun>
    val x : int = 120|}]
;;

let%expect_test _ =
  run_inter {|
      let x = [ 5 ; 6 ]
       |};
  [%expect {| 
        val x : int list = [5; 6]
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = true || false && true
       |};
  [%expect {| 
        val x : bool = true
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = y / 1
       |};
  [%expect {| 
        Typecheck error: Undefined variable 'y'
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = 100 / 0
       |};
  [%expect {| 
        Interpreter error: Division by zero
      |}]
;;

let%expect_test _ =
  run_inter
    {|
      let mymatch x =
        match x with
          | hd :: tl -> hd
          | _ -> 0

      let a = mymatch []
       |};
  [%expect
    {| 
         val a : int = 0
         val mymatch : int list -> int = <fun>
      |}]
;;

let%expect_test _ =
  run_inter
    {|
      let mymatch x =
        match x with
          | hd :: tl -> tl
          | _ -> 0

      let a = mymatch [1; 2; 3]
       |};
  [%expect
    {| 
         val a : int = [2; 3]
         val mymatch : int list -> int = <fun>
      |}]
;;
