(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Factorial. *)

let%expect_test _ =
  interpret
    {| let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
let res = fact 5;;|};
  [%expect {|
    val fact : int -> int = <fun>
    val res : int = 120 |}]
;;

(* ---------------- *)

(* Fibonacci. *)

let%expect_test _ =
  interpret
    {| let rec fib n = if n > 1 then fib (n - 1) + fib (n - 2) else 1;;
let res = fib 5;;|};
  [%expect {|
    val fib : int -> int = <fun>
    val res : int = 8 |}]
;;

(* ---------------- *)

let%expect_test _ =
  interpret
    {| let list_map f =
      let rec helper l = match l with
      | [] -> []
      | h :: tl -> (f h) :: helper tl
    in helper 
    ;;

    let increment x = x + 1;;

    let test_list = [2; 4; 5; 3; 6; 3;];;

    let res = list_map increment test_list
    |};
  [%expect
    {|
    val list_map : ('a -> 'b) -> 'c list -> 'd list = <fun>
    val increment : int -> int = <fun>
    val test_list : int list = [2; 4; 5; 3; 6; 3]
    val res : 'a list = [3; 5; 6; 4; 7; 4] |}]
;;

let%expect_test _ =
  interpret
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

    let res1 = f [ 1 ; 1 ; 0 ; 1 ; 1 ; 0 ; 1 ; 1 ; 0 ; 0 ; 1 ]
    let res2 = f [ 1 ; 0 ; 1 ; 3 ; 0 ; 1 ; 1 ]
    let res3 = f []
  |};
  [%expect
    {|
    val g : int -> bool = <fun>
    val f : int list -> bool = <fun>
    val res1 : bool = true
    val res2 : bool = false
    val res3 : bool = true |}]
;;
