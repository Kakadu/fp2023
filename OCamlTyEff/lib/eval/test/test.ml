(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Run

let%expect_test _ =
  run {| let () = print_endline "Hello, World!" |} ;
  [%expect {| Hello, World! |}]

let%expect_test _ =
  run
    {|
      let rec fact n = if n < 2 then 1 else n * fact (n - 1);;
      fact 5
    |} ;
  [%expect {|
    fact: <fun>
    120 |}]

let%expect_test _ =
  run {|
        let a = 1 and b = 2 in
        (fun (x, y) -> x + y) (a, b) |} ;
  [%expect {| 3 |}]

let%expect_test _ =
  run {| (fun f x y -> f x y ) (+) 1 2 |} ;
  [%expect {| 3 |}]

let%expect_test _ =
  run {| let (*) (Some x) (Some y) = x - y in
         (Some 2) * (Some 4) |} ;
  [%expect {| -2 |}]

let%expect_test _ =
  run
    {|
      let f = function
        | Some x -> x
        | None -> 0
      in
      f None, f (Some 42) |} ;
  [%expect {| (0, 42) |}]

let%expect_test _ =
  run
    {|
      let f x = match x with 0 -> None | 1 -> Some x;;
      f 0;; f 1;; f 5;;
    |} ;
  [%expect {|
    f: <fun>
    None
    Some 1
    Exception: Match_failure |}]

let%expect_test _ =
  run {| match 0 with
         | 0 -> let 1 = 2 in 1
         | _ -> 2 |} ;
  [%expect {| Exception: Match_failure |}]

let%expect_test _ =
  run
    {|
      let (/) x y = try x / y with Division_by_zero -> 0 in
      4 / 0, 4 / 2;;

      4 / 0
    |} ;
  [%expect {|
    (0, 2)
    Exception: Division_by_zero |}]

let%expect_test _ =
  run
    {|
      let rec map f = function
        | [] -> []
        | h :: tl -> f h :: map f tl
       in
       map ((+) 1) [1;2;3;4]
    |} ;
  [%expect {| :: (2, :: (3, :: (4, :: (5, [])))) |}]

let%expect_test _ =
  run
    {|
      exception My_exc of int;;
      let catch f = try f () with My_exc num -> print_int num;;

      catch (fun () -> raise Division_by_zero);;
      catch (fun () -> raise @@ My_exc 42)
    |} ;
  [%expect {|
    catch: <fun>
    Exception: Division_by_zero
    42() |}]

let%expect_test _ =
  run {|
    let a = ref 5;;
    !a;;
    a := 42;; !a |} ;
  [%expect {|
    a: ref { 5 }
    5
    ()
    42 |}]

let%expect_test _ =
  run
    {|
      let x = ref 5 in
      let f arg = arg + !x in
      x := 20; f 10
    |} ;
  [%expect {| 30 |}]

let%expect_test _ =
  run
    {|
      let a = ref None;;
      let b = a;;

      b := Some 1;; !a;;
      a := Some 42;; !b
    |} ;
  [%expect
    {|
    a: ref { None }
    b: ref { None }
    ()
    Some 1
    ()
    Some 42 |}]

let%expect_test _ =
  run
    {|
      let f =
        let cache = ref None in
        fun x -> match !cache with
          | Some cached -> cached
          | None -> cache := Some x; x;;

      f 42;; f 10
    |} ;
  [%expect {|
    f: <fun>
    42
    42 |}]

let%expect_test _ =
  run
    {|
      let f =
        let x = ref 5 in
        fun () -> x := !x + 1; !x;;
      f (); f()
    |} ;
  [%expect {|
    f: <fun>
    7 |}]
