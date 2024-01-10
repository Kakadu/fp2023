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

let%expect_test _ =
  run
    {|
    let catch_exc1 f x = try f x with Exc1 -> ();;

    let f1 _ = raise Exc1;;
    catch_exc1 f1;;

    let f2 = function 1 -> raise Exc1 | 2 -> raise Exc2 | _ -> ();;
    catch_exc1 f2;;

    catch_exc1 print_string;;
    fun _ -> catch_exc1 raise Exc1
    |} ;
  [%expect
    {|
    catch_exc1: 'a 'e. ('a -[exn _Exc1 | 'e]-> unit) -> 'a -'e-> unit
    f1: 'a 'b. 'a -[exn _Exc1]-> 'b
    'a. 'a -> unit
    f2: int -[exn _Exc2, exn _Exc1]-> unit
    int -[exn _Exc2]-> unit
    string -[console]-> unit
    'a. 'a -[exn _Exc1]-> unit |}]

let%expect_test _ =
  run
    {|
    let catch f x = try f x with Exc1 -> raise Exc1 | Exc2 -> print_string "exc2";;

    catch (fun _ -> raise Exc1; raise Exc2);;
    catch (fun _ -> raise Exc1);;
    catch id |} ;
  [%expect
    {|
      catch: 'a 'e. ('a -[exn _Exc1, exn _Exc2, console, exn _Exc1 | 'e]-> unit) -> 'a -[console, exn _Exc1 | 'e]-> unit
      'a. 'a -[console, exn _Exc1]-> unit
      'a. 'a -[console, exn _Exc1]-> unit
      unit -[console, exn _Exc1]-> unit |}]

let%expect_test _ =
  run
    {|
    exception My_exc1;;
    exception My_exc2;;
    let catch f x = try print_string "hi"; f x with My_exc1 -> 1 | My_exc2 -> 2 | Exc1 -> 0 |} ;
  [%expect
    {|
    My_exc1: _My_exc1 exception
    My_exc2: _My_exc2 exception
    catch: 'a 'e. ('a -[exn _My_exc1, exn _My_exc2, exn _Exc1, console | 'e]-> int) -> 'a -[console | 'e]-> int |}]
