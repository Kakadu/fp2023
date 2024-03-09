(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List contructions *)

let%expect_test _ =
  interpret {| 
    (1 :: 2 :: []) :: (1 :: 2 :: []) :: []
  |};
  [%expect {|
    - : int list list = [[1; 2]; [1; 2]] |}]
;;

let%expect_test _ =
  interpret {| 
    1 :: 'c' :: []
  |};
  [%expect
    {|
    Type error: unification failed - type char does not match expected type int |}]
;;

(* ---------------- *)

(* List *)

let%expect_test _ =
  interpret {| 
    [1 ; 2 ; 3]
  |};
  [%expect {|
    - : int list = [1; 2; 3] |}]
;;

let%expect_test _ =
  interpret {| 
    [(1, 2) ; (2, 3); (3, 4)]
  |};
  [%expect {|
    - : int * int list = [(1, 2); (2, 3); (3, 4)] |}]
;;

(* ---------------- *)

(* Tuple *)

let%expect_test _ =
  interpret {| 
    let f = (1, 2 , 3)
  |};
  [%expect {|
    val f : int * int * int = (1, 2, 3) |}]
;;

let%expect_test _ =
  interpret {| 
    let f = (1, (1, 2 :: 3 :: 4 :: 5 :: []) , "some string")
  |};
  [%expect
    {|
    val f : int * (int * int list) * string = (1, (1, [2; 3; 4; 5]), "some string") |}]
;;

(* ---------------- *)
