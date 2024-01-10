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
    let foo f = print_string "hi"; f ();;
    fun _ -> foo raise;;
    fun _ -> foo id |} ;
  [%expect
    {|
    'a 'e. (unit -[console | 'e]-> 'a) -[console | 'e]-> 'a
    foo: 'a 'e. (unit -[console | 'e]-> 'a) -[console | 'e]-> 'a
    'a 'b. 'a -[console, exn]-> 'b
    'a. 'a -[console]-> unit |}]

let%expect_test _ =
  run {|
    let id = fun x -> x in
    id print_string; id raise; id |} ;
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test _ =
  run {|
      let foo f g _ = f ""; g () in
      foo print_string raise
    |} ;
  [%expect {| 'a 'b. 'a -[console, exn]-> 'b |}]

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
    'a 'b 'e. ('a -'e-> 'b) -'e-> 'a list -'e-> 'b list
    map: 'a 'b 'e. ('a -'e-> 'b) -'e-> 'a list -'e-> 'b list
    'a. 'a list -> 'a list
    string list -[console]-> unit list |}]

let%expect_test _ =
  run {| fun f -> f 1 2 |} ;
  [%expect {| 'a 'e. (int -'e-> int -'e-> 'a) -'e-> 'a |}]

let%expect_test _ =
  run {| let rec foo f x = f x; foo f x in foo |} ;
  [%expect {| 'a 'b 'c 'e. ('a -'e-> 'b) -'e-> 'a -'e-> 'c |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> x + y |} ;
  [%expect {| int -[console]-> int -> int |}]

let%expect_test _ =
  run {| fun x -> raise (); fun y -> raise (); x + y |} ;
  [%expect {| int -[exn]-> int -[exn]-> int |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> raise (); x + y |} ;
  [%expect {| int -[console]-> int -[exn]-> int |}]
