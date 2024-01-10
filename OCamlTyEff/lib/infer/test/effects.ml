(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Run

let%expect_test _ =
  run {| fun f g -> f (); g (); print_string "42" |} ;
  [%expect
    {| 'a 'b 'e. (unit -[console | 'e]-> 'a) -> (unit -[console | 'e]-> 'b) -[console | 'e]-> unit |}]

let%expect_test _ =
  run
    {|
    let foo f = print_string "hi"; f Exc1;;
    fun _ -> foo raise;;
    fun _ -> foo id |} ;
  [%expect
    {|
    foo: 'a 'e. (_Exc1 exception -[console | 'e]-> 'a) -[console | 'e]-> 'a
    'a 'b. 'a -[console, exn _Exc1]-> 'b
    'a. 'a -[console]-> _Exc1 exception |}]

let%expect_test _ =
  run {|
    let id = fun x -> x in
    id print_string; id raise; id |} ;
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test _ =
  run
    {|
      let foo f g _ = f ""; g Exc1 in
      foo print_string raise
    |} ;
  [%expect {| 'a 'b. 'a -[console, exn _Exc1]-> 'b |}]

let%expect_test _ =
  run
    {| let rec map f = function
         | [] -> []
         | x::xs -> (f x) :: (map f xs)
       ;;

       map id;;
       map print_string |} ;
  [%expect
    {|
    map: 'a 'b 'e. ('a -'e-> 'b) -> 'a list -'e-> 'b list
    'a. 'a list -> 'a list
    string list -[console]-> unit list |}]

let%expect_test _ =
  run {| fun f -> f 1 2 |} ;
  [%expect {| 'a 'e. (int -'e-> int -'e-> 'a) -'e-> 'a |}]

let%expect_test _ =
  run {| let rec foo f x = f x; foo f x in foo |} ;
  [%expect {| 'a 'b 'c 'e. ('a -'e-> 'b) -> 'a -'e-> 'c |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> x + y |} ;
  [%expect {| int -[console]-> int -> int |}]

let%expect_test _ =
  run {| fun x -> raise Exc1; fun y -> raise Exc1; x + y |} ;
  [%expect {| int -[exn _Exc1]-> int -[exn _Exc1]-> int |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> raise Exc2; x + y |} ;
  [%expect {| int -[console]-> int -[exn _Exc2]-> int |}]

let%expect_test _ =
  run {| function 1 -> raise Exc1 | 2 -> raise Exc2 | _ -> 0 |} ;
  [%expect {| int -[exn _Exc2, exn _Exc1]-> int |}]
