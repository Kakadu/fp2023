(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List Construction pattern *)

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | hd :: tl -> hd
      | [] -> 0
    ;;
    |};
  [%expect {| val f : int list -> int |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | x :: y :: z -> x + y
      | _ -> 0
    |};
  [%expect {|
    val f : int list -> int |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | x :: y :: z -> x + y
      | _ -> 0
    
    let res = f [true ; false ; true ; false ; true]
    |};
  [%expect
    {|
    Type error: unification failed - type int does not match expected type bool |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = match x with
    | x :: y :: x -> 0
    | _ -> 0
    ;;
    |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = match x with
    | x :: (y :: d :: (l :: y :: x)) -> 0
    | _ -> 0
    ;;
    |};
  [%expect {| Type error: variable 'y' is bound several times |}]
;;

(* ---------------- *)

(* Tuple pattern *)

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | (x, y) -> x
      | _ -> true
    ;;
    |};
  [%expect {| val f : bool * 'a -> bool |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | (x, y) -> false
      | _ -> true
    ;;
    |};
  [%expect {| val f : 'a * 'b -> bool |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | (x, y, x) -> 0
      | _ -> 0
    ;;
    |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference
    {|
    let f x = 
      match x with
      | (x, y, (z,q,(c,k,(l,x)))) -> 0
      | _ -> 0
    ;;
    |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference
    {|
    let binary x y = 
      match (x,y) with
      | (0, 1) -> true
      | (1, 0) -> true
      | _ -> false
    |};
  [%expect {|
    val binary : int -> int -> bool |}]
;;

let%expect_test _ =
  inference
    {|
    let f x y z = 
      match (x, y, z) with
      | (a, (b :: d :: l), (j, r, t)) -> d + r
      | _ -> 1
    |};
  [%expect {|
    val f : 'a -> int list -> 'b * int * 'c -> int |}]
;;

let%expect_test _ =
  inference
    {|
    let f x =
      match x with
      | (x, y) -> x + y
      | _ -> 0
    ;;

    let res = f ((1,2), (1,2))
    |};
  [%expect
    {|
    Type error: unification failed - type int does not match expected type int * int |}]
;;

let%expect_test _ =
  inference
    {|
    let f x y z = 
      match (x, y, z) with
      | (a, (b :: d :: l), (j, d, t)) -> 0
      | _ -> 1
    |};
  [%expect {|
    Type error: variable 'd' is bound several times |}]
;;

(* ---------------- *)

(* Effects patterns *)

let%expect_test _ =
  inference
    {|
      effect E : char * int * string -> int effect
  
      let f x = 
        match x with
        | E (x, y, z) -> x
        | _ -> 'a'
      ;;
      |};
  [%expect
    {|
      val E : char * int * string -> int effect
      val f : (char * int * string -> int effect) -> char |}]
;;

let%expect_test _ =
  inference
    {|
      effect E : char * int * string -> int effect
  
      let f x = 
        match x with
        | E (x, y, x) -> x
        | _ -> 'a'
      ;;
      |};
  [%expect {|
      Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference
    {|
    effect E : int effect

    let f x = 
      match x with
      | E -> 0
      | _ -> 0
    ;;
    |};
  [%expect {|
    val E : int effect
    val f : int effect -> int |}]
;;

let%expect_test _ =
  inference
    {|
    effect E : int -> int effect

    let f x = 
      match x with
      | E x -> 0
      | _ -> 0
    ;;
    |};
  [%expect {|
    val E : int -> int effect
    val f : (int -> int effect) -> int |}]
;;

let%expect_test _ =
  inference
    {|
    effect E : int -> int effect

    let f x = 
      match x with
      | E (x :: y) -> x
      | _ -> 0
    ;;
    |};
  [%expect
    {|
    Type error: unification failed - type 'a list does not match expected type int |}]
;;

let%expect_test _ =
  inference
    {|
      effect E : int list -> int effect
  
      let f x = 
        match x with
        | E (x :: y :: x) -> x
        | _ -> 0
      ;;
      |};
  [%expect {|
      Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference
    {|
      effect E : int list -> int effect
  
      let f x = 
        match x with
        | E (x :: y) -> x
        | _ -> 0
      ;;
      |};
  [%expect
    {|
      val E : int list -> int effect
      val f : (int list -> int effect) -> int |}]
;;

let%expect_test _ =
  inference
    {|
    effect E : int effect

    let f x = 
      match x with
      | E x -> 1
      | _ -> 0
    ;;
    |};
  [%expect
    {|
    Type error: effect 'E' cannot take arguments - it is an effect without arguments. |}]
;;

(* ---------------- *)
