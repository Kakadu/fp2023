(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Inter

let%expect_test _ =
  run_inter {|
     let x = 5 + 6 - 3 * 7 / 1
    |};
  [%expect {|x: -10|}]
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
    mymatch: <fun>
    result: false |}]
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
    a: 1
    mymatch: <fun> |}]
;;

let%expect_test _ =
  run_inter {|
     let f a = if a < 1 then true else false

     let x = f 0
    |};
  [%expect {|
    f: <fun>
    x: true |}]
;;

let%expect_test _ =
  run_inter
    {|
     let rec fact x = if x < 1 then 1 else x * fact (x - 1)

     let x = fact 5
    |};
  [%expect {|
    fact: <fun>
    x: 120|}]
;;

let%expect_test _ =
  run_inter {|
      let x = [ 5 ; 6 ]
       |};
  [%expect {| 
        x: [5; 6]
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = true || false && true
       |};
  [%expect {| 
        x: true
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
  [%expect {| 
         a: 0
         mymatch: <fun>
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
  [%expect {| 
         a: [2; 3]
         mymatch: <fun>
      |}]
;;
