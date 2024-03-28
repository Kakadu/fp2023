(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Inter

let%expect_test _ =
  run_inter {|
      let x = [ 5 ; 6 ]
       |};
  [%expect {| 
        [5; 6]
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = 5 + 6 - 7 * 9 / 1
       |};
  [%expect {| 
        -52
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = true 
       |};
  [%expect {| 
        true
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = false 
       |};
  [%expect {| 
        false
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = true || false && true
       |};
  [%expect {| 
        true
      |}]
;;

let%expect_test _ =
  run_inter
    {|
      let rec fact x = if x < 1 then 1 else x * fact (x - 1)
      let y = fact 5
       |};
  [%expect {| 
        <fun>
        120
      |}]
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
        <fun>
        false
      |}]
;;

let%expect_test _ =
  run_inter {|
      let f x y = if x = y then 100 else 33

      let a = f 5 4
       |};
  [%expect {| 
        <fun>
        33
      |}]
;;

let%expect_test _ =
  run_inter {|
      let f x y = if x = y then 100 else 33

      let a = f 5 4
       |};
  [%expect {| 
        <fun>
        33
      |}]
;;

let%expect_test _ =
  run_inter {|
      let f x y = if x < y then 100 else 33

      let a = f 5 4
       |};
  [%expect {| 
        <fun>
        33
      |}]
;;

let%expect_test _ =
  run_inter {|
      let f x y = if x > y then 100 else 33

      let a = f 5 4
       |};
  [%expect {| 
        <fun>
        100
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = 100 / 0
       |};
  [%expect {| 
        Interpretation error
      |}]
;;

let%expect_test _ =
  run_inter {|
      let x = y / 1
       |};
  [%expect {| 
        Typecheck error
      |}]
;;

let%expect_test _ =
  run_inter
    {|
      let mymatch x =
        match x with
          | hd :: [] -> hd
          | _ -> 0
       |};
  [%expect {| 
         <fun>
      |}]
;;
